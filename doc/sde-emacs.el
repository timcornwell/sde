
(defun revise-sde-header  "Massage the header of an SDE subroutine or function into a 
standard format. Only fixes first in a file."
   (interactive (beginning-of-buffer)
   (replace-regexp "^CA" "C")
   (replace-regexp "^CD" "C")
   (replace-regexp "^CE" "C")
   (replace-regexp "^CS" "C")
   (replace-regexp "^C Arguments:.*$" "C")
   (beginning-of-buffer)
   (search-forward "National Radio")
   (forward-line -1)
   (beginning-of-line)
   (let ((beg (point))) 
   (re-search-forward "SUBROUTINE\\|FUNCTION")
   (beginning-of-line)
   (delete-region beg (point)))
   (insert "C
C	National Radio Astronomy Observatory, Socorro, NM 87801
C	Software Development Environment (SDE)
C++
C %W%    %G%
C
")
   (beginning-of-buffer)
   (re-search-forward "SUBROUTINE\\|FUNCTION")
   (re-search-forward "^C .*$")
   (beginning-of-line)
   (forward-char)
   (insert "D")))