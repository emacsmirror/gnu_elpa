//
// Copyright (C) 2025-2026  Divya Ranjan Pattanaik
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include "mupdf-helpers.h"
#include "render-threads.h"
ThreadPool g_thread_pool;

/**
 * Initialize the job queue.
 *
 * Sets head and tail to NULL and initializes the mutex and condition variable.
 *
 * @param queue  Pointer to the JobQueue to initialize.
 */

void
job_queue_init(JobQueue *queue)
{
	queue->head = queue->tail = NULL;
	pthread_mutex_init(&queue->q_mutex, NULL);
	pthread_cond_init(&queue->q_cond, NULL);
}

/**
 * Destroy the job queue.
 *
 * Destroys the associated mutex and condition variable.
 *
 * @param queue  Pointer to the JobQueue to destroy.
 */

void
job_queue_destroy(JobQueue *queue)
{
	pthread_mutex_destroy(&queue->q_mutex);
	pthread_cond_destroy(&queue->q_cond);
}

/**
 * Push a job into the job queue.
 *
 * Appends the job to the end of the queue and signals waiting threads.
 *
 * @param queue  Pointer to the JobQueue.
 * @param job    Pointer to the ThreadJob to enqueue.
 */

void
job_queue_push(JobQueue *queue, ThreadJob *job)
{
	job->next = NULL;
	pthread_mutex_lock(&queue->q_mutex);
	if (queue->tail)
		queue->tail->next = job;
	else
		queue->head = job;
	queue->tail = job;
	pthread_cond_signal(&queue->q_cond);
	pthread_mutex_unlock(&queue->q_mutex);
}

/**
 * Pop a job from the job queue.
 *
 * Blocks until a job is available, then removes and returns it.
 *
 * @param queue  Pointer to the JobQueue.
 * @return       Pointer to the next ThreadJob.
 */

ThreadJob *
job_queue_pop(JobQueue *queue)
{
	pthread_mutex_lock(&queue->q_mutex);
	while (!queue->head || queue->head == NULL) // Waiting for work
		pthread_cond_wait(&queue->q_cond, &queue->q_mutex);
	ThreadJob *job = queue->head;
	queue->head = job->next;
	if (!queue->head) // If there's no next job, set NULL
		queue->tail = NULL;
	pthread_mutex_unlock(&queue->q_mutex);
	return job;
}

/**
 * Worker thread routine for the thread pool.
 *
 * Continuously fetches and runs jobs from the job queue, using a
 * leader/follower pattern to manage scheduling. Terminates on receiving a job
 * with a NULL function.
 *
 * @param arg  Pointer to the ThreadPool.
 * @return     NULL on termination.
 */

void *
thread_routine(void *arg)
{
	ThreadPool *pool = (ThreadPool *)arg;

	while (1)
	{
		// Try to become the leader
		pthread_mutex_lock(&pool->leader_mutex);
		while (pool->leader_exists)
		{
			pthread_cond_wait(&pool->leader_cond,
					  &pool->leader_mutex);
		}

		// Become leader
		pool->leader_exists = 1;
		pthread_mutex_unlock(&pool->leader_mutex);

		// Fetch a task
		ThreadJob *job = job_queue_pop(&pool->job_queue);

		if (!job || job == NULL)
		{
			// Hand off leadership and continue loop so other
			// threads can become leader
			pthread_mutex_lock(&pool->leader_mutex);
			pool->leader_exists = 0;
			pthread_cond_signal(&pool->leader_cond);
			pthread_mutex_unlock(&pool->leader_mutex);
			continue;
		}

		// Hand off leadership
		pthread_mutex_lock(&pool->leader_mutex);
		pool->leader_exists = 0;
		pthread_cond_signal(&pool->leader_cond);
		pthread_mutex_unlock(&pool->leader_mutex);

		if (!job->func)
		{
			free(job);
			break;
		}

		job->func(job->arg);
	}

	return NULL;
}

/**
 * Initialize the thread pool.
 *
 * Initializes the leader lock, job queue, and spawns worker threads.
 *
 * @param pool  Pointer to the ThreadPool to initialize.
 */

void
threadpool_init(ThreadPool *pool)
{
	pool->leader_exists = 0;
	pthread_mutex_init(&pool->leader_mutex, NULL);
	pthread_cond_init(&pool->leader_cond, NULL);
	job_queue_init(&pool->job_queue);

	for (int i = 0; i < MAX_POOL_SIZE; i++)
		pthread_create(&pool->threads[i], NULL, thread_routine, pool);
}

/**
 * Shutdown the thread pool.
 *
 * Joins all threads and destroys the job queue and synchronization primitives.
 *
 * @param pool  Pointer to the ThreadPool to shut down.
 */

void
threadpool_shutdown(ThreadPool *pool)
{
	fprintf(stderr,
		"Joining %d threads and shutting down the thread pool...\n",
		MAX_POOL_SIZE);
	for (int i = 0; i < MAX_POOL_SIZE; i++)
		pthread_join(pool->threads[i], NULL);
	fprintf(stderr, "Destroying the thread job queue..\n");
	job_queue_destroy(&pool->job_queue);
	pthread_mutex_destroy(&pool->leader_mutex);
	pthread_cond_destroy(&pool->leader_cond);
}

// Thread job helpers

/**
 * Submit a job to the thread pool.
 *
 * Wraps the given function and argument into a ThreadJob and pushes it to the
 * queue.
 *
 * @param func  Function pointer representing the job.
 * @param arg   Argument to pass to the job function.
 * @param pool  Pointer to the ThreadPool to submit the job to.
 */

void
submit_job(void *(*func)(void *arg), void *arg, ThreadPool *pool)
{
	ThreadJob *job = (ThreadJob *)malloc(sizeof(*job));
	job->func = func;
	job->arg = arg;
	job_queue_push(&pool->job_queue, job);
}

/* Submit a draw_page_thread render job */

bool
submit_render_job(DocState *doc_state,
                  EmacsWinState *win_state,
                  CachedPage *cp,
                  DocPage *dp)
{
	if (!doc_state || !win_state || !cp || !dp)
		return false;

	// Job-owned DocPage reference
	atomic_fetch_add(&dp->refcount, 1);

	// Count the job at submission time, not at start time
	atomic_fetch_add(&doc_state->active_jobs, 1);
	atomic_fetch_add(&win_state->active_jobs, 1);

	DrawThreadArgs *args = malloc(sizeof(DrawThreadArgs));

	if (!args)
		{
			release_doc_page(doc_state, dp);
			atomic_fetch_sub(&doc_state->active_jobs, 1);
			atomic_fetch_sub(&win_state->active_jobs, 1);
			return false;
		}

	args->doc_state = doc_state;
	args->win_state = win_state;
	args->cp        = cp;
	args->dp        = dp;

	pthread_mutex_lock(&cp->mutex);
	cp->status = PAGE_STATUS_RENDERING;
	pthread_mutex_unlock(&cp->mutex);

	submit_job(draw_page_thread, args, &g_thread_pool);
	return true;
}

void
wait_for_win_jobs(EmacsWinState *win_state)
{
	if (!win_state)
		return;

	while (atomic_load(&win_state->active_jobs) > 0)
		{
			struct timespec ts = {0, 1000000};
			nanosleep(&ts, NULL);
		}
}

void
wait_for_doc_jobs(DocState *doc_state)
{
	if (!doc_state)
		return;

	while (atomic_load(&doc_state->active_jobs) > 0)
		{
			struct timespec ts = {0, 1000000};
			nanosleep(&ts, NULL);
		}
}

bool
wait_for_cached_page(CachedPage *cp)
{
	if (!cp)
		return false;

	pthread_mutex_lock(&cp->mutex);

	while (cp->status == PAGE_STATUS_EMPTY ||
	       cp->status == PAGE_STATUS_RENDERING)
		{
			pthread_cond_wait(&cp->cond, &cp->mutex);
		}

	bool ok = (cp->status == PAGE_STATUS_READY &&
		   cp->img_data != NULL &&
		   cp->img_size > 0);

	pthread_mutex_unlock(&cp->mutex);

	return ok;
}
