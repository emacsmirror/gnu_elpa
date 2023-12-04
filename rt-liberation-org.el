;;; rt-liberation-org.el --- Org integration for rt-liberation  -*- lexical-binding: t; -*-

;; Copyright (C) 2008-2021 Free Software Foundation, Inc.

;; Author: Yuchen Pei <hi@ypei.me>
;; Authors: Yoni Rabkin <yrk@gnu.org>
;; Maintainer: Yoni Rabkin <yrk@gnu.org>

;; This file is a part of rt-liberation.
 
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

(require 'org)
(require 'rt-liberation)

(defun rt-org-open (link)
  "Opens the rt LINK.
	Open the rt ticket (for links starting with `id:') or run
	the query (for links starting with `query:')."
  (cond
   ((string-match "^id:\\([^/]+\\)\\(/\\(.+\\)\\)?" link)
    (let ((id (match-string 1 link))
	  (history-id (match-string 3 link)))
      (rt-liber-browse-query (format "id = \"%s\"" id))
      (rt-liber-browser-move-point-to-ticket id)
      (rt-liber-ticket-at-point)
      (when history-id 
	  (rt-liber-viewer-move-point-to-section history-id))))
   ((string-match "^query:\\(.+\\)" link)
    (rt-liber-browse-query (match-string 1 link)))
   (t (error "Unrecognized link type '%s'" link))))

(defun rt-org-store-link ()
  "Stores an rt link in ticket-browser or ticker-viewer mode."
  (let ((ticket (or (get-text-property (point) 'rt-ticket)
		    rt-liber-ticket-local)))
    (when ticket
      (let* ((ticket-id-link (concat "rt:id:" (rt-liber-ticket-id-only ticket)))
	     (subject (cdr (assoc "Subject" ticket)))
	     (history-id (alist-get 'id (rt-liber-viewer-get-section-data)))
	     (link (concat ticket-id-link (if history-id (concat "/" history-id) ""))))
	(org-link-add-props
	 :link link
	 :description subject)
	link))))

(if (fboundp 'org-link-set-parameters)
    (org-link-set-parameters "rt"
			     :follow #'rt-org-open
			     :store  #'rt-org-store-link)
  (error "org-link-set-parameters is void. Are you using an old version of Org?"))

;;; _
(provide 'rt-liberation-org)
;;; rt-liberation-org.el ends here
