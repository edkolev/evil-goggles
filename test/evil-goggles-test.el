
(require 'ert)
(require 'evil)
(require 'evil-goggles)
(require 'evil-test-helpers)

(evil-goggles-mode)

(ert-deftest evil-goggles-test ()
  :tags '(evil-goggles)
  (ert-info ("delete interactive")
    (evil-test-buffer
      "line"
      ("dd")
      ""))
  (ert-info ("delete non-interactive")
    (evil-test-buffer
      "line"
      (evil-delete (point-min) (point-max))
      "")))

(ert-deftest evil-test-last-insert-register ()
  "Test last insertion register."
  (evil-test-buffer
   "[l]ine 1\n"
   ("GiABC" [escape])
   "line 1\nAB[C]"
   ("gg\".P")
   "AB[C]line 1\nABC"))
