;;; seijiseikana-test.el --- Unit test for seijiseikana.el -*- coding: utf-8 -*-

;; Copyright (C) 2012  MORIYAMA Hiroshi

;; Author: MORIYAMA Hiroshi <hiroshi@kvd.biglobe.ne.jp>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; ert.el:
;; <http://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/emacs-lisp/ert.el>.

;; Usage:
;; % emacs --directory . --batch --eval '(load-file "seijiseikana-test.el")'

;;; Code:

(require 'ert)
(require 'seijiseikana)

;;; Convert Region

(ert-deftest test-seijiseikana-seiji-region ()
  (let ((target-text "歴史的仮名遣。")
        (expected "歴史的假名遣。"))
    (should (equal expected
                   (with-temp-buffer
                     (insert target-text)
                     (seijiseikana-seiji-region (point-min) (point-max))
                     (buffer-substring-no-properties (point-min)
                                                     (point-max)))))))

(ert-deftest test-seijiseikana-ryakuji-region ()
  (let ((target-text "歴史的假名遣。")
        (expected "歴史的仮名遣。"))
    (should (equal expected
                   (with-temp-buffer
                     (insert target-text)
                     (seijiseikana-ryakuji-region (point-min) (point-max))
                     (buffer-substring-no-properties (point-min)
                                                     (point-max)))))))

;;; Save Match Data

(defun seijiseikana-test-run-region-function (function &optional target-text)
  (let ((target-text (or target-text "<title>試験</title>")))
    (with-temp-buffer
      (insert target-text)
      (funcall function (point-min) (point-max)))))

(ert-deftest test-save-match-data-in-seijiseikana-seiji-region ()
  (should (equal (match-data)
                 (progn
                   (seijiseikana-test-run-region-function
                    #'seijiseikana-seiji-region)
                   (match-data)))))

(ert-deftest test-save-match-data-in-seijiseikana-ryakuji-region ()
  (should (equal (match-data)
                 (progn
                   (seijiseikana-test-run-region-function
                    #'seijiseikana-ryakuji-region)
                   (match-data)))))


(ert-run-tests-batch-and-exit)
;;; seijiseikana-test.el ends here
