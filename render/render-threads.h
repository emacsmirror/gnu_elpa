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

#ifndef RENDER_THREADS_H
#define RENDER_THREADS_H

#include "render-core.h"
#include <pthread.h>

#define MAX_POOL_SIZE 8 // Max no. of threads to be used

typedef struct ThreadJob
{
	void *(*func)(void *arg);
	void *arg;
	struct ThreadJob *next;
} ThreadJob;

typedef struct TaskQueue
{
	ThreadJob *head, *tail;
	pthread_mutex_t q_mutex;
	pthread_cond_t q_cond;
} JobQueue;

typedef struct
{
	pthread_t threads[MAX_POOL_SIZE];
	pthread_mutex_t leader_mutex;
	pthread_cond_t leader_cond;
	int leader_exists;
	JobQueue job_queue;
} ThreadPool;

typedef struct
{
	DocState *doc_state;
	EmacsWinState *win_state;
	CachedPage *cp;
	DocPage *dp;
} DrawThreadArgs;

extern ThreadPool g_thread_pool;

void
job_queue_init(JobQueue *queue);
void
job_queue_destroy(JobQueue *queue);
void
job_queue_push(JobQueue *queue, ThreadJob *job);
ThreadJob *
job_queue_pop(JobQueue *queue);

void
submit_job(void *(*func)(void *arg), void *arg, ThreadPool *pool);
bool
submit_render_job(DocState *doc_state, EmacsWinState *win_state,
		  CachedPage *cp, DocPage *dp);
void
wait_for_win_jobs(EmacsWinState *win_state);
void
wait_for_doc_jobs(DocState *doc_state);
bool
wait_for_cached_page(CachedPage *cp);

void *
thread_routine(void *arg);
void
threadpool_init(ThreadPool *pool);
void
threadpool_shutdown(ThreadPool *pool);

#endif
