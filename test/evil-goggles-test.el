
(require 'ert)
(require 'evil)
(require 'evil-goggles)
(require 'evil-test-helpers)

(evil-goggles-mode)

(defface evil-goggles--test-face '((t (:inherit region))) "Evil-goggles test face.")

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

(ert-deftest evil-goggles-test-show-or-pulse ()
  "Test evil-goggles--should-blink-or-pulse - whether to pulse or blink "
  (let ((evil-goggles-pulse t))
    ;; enabled pusling, use fg-only - hints shouldn't pulse, hints should be fg only (because pulsing requires a bg)
    (progn
      (custom-set-faces
       '(evil-goggles--test-face ((t (:foreground "red")))))
      (should (equal
               (evil-goggles--should-blink-or-pulse 'evil-goggles--test-face)
               '(do-blink evil-goggles--test-face))))

    ;; enabled pusling, use bg-only - hints should pulse with the bg color
    (progn
      (custom-set-faces
       '(evil-goggles--test-face ((t (:background "red")))))
      (should (equal
               (evil-goggles--should-blink-or-pulse 'evil-goggles--test-face)
               '(do-pulse "red"))))

    ;; enabled pulsing, use an invalid face (no fg, no bg) - hints should pulse with default face's bg
    (progn
      (custom-set-faces
       '(evil-goggles--test-face ((t (:underline nil)))))
      (should (equal
               (evil-goggles--should-blink-or-pulse 'evil-goggles--test-face)
               `(do-pulse ,(face-background 'evil-goggles-default-face nil t))))))

  (let ((evil-goggles-pulse nil))
    ;; disabled pusling, use bg-only - hints shouldn't pulse, only bg
    (progn
      (custom-set-faces
       '(evil-goggles--test-face ((t (:background "red")))))
      (should (equal
               (evil-goggles--should-blink-or-pulse 'evil-goggles--test-face)
               '(do-blink evil-goggles--test-face))))

    ;; disabled pusling, use fg-only - hints shouldn't pulse, only fg
    (progn
      (custom-set-faces
       '(evil-goggles--test-face ((t (:foreground "red")))))
      (should (equal
               (evil-goggles--should-blink-or-pulse 'evil-goggles--test-face)
               '(do-blink evil-goggles--test-face))))

    ;; disalbed pulsing, use an invalid face (no fg, no bg) - hints should blink with default face
    (progn
      (custom-set-faces
       '(evil-goggles--test-face ((t (:underline nil)))))
      (should (equal
               (evil-goggles--should-blink-or-pulse 'evil-goggles--test-face)
               '(do-blink evil-goggles-default-face))))))

(ert-deftest evil-goggles-test-substitute ()
  (ert-info ("Test visual substitute")
    (evil-test-buffer
      ";; [T]his buffer is for notes."
      ("v3lcABC" [escape])
      ";; AB[C] buffer is for notes.")))
