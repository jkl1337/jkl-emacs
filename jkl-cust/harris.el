
(defvar hft/red-port
  (if (eq system-type 'windows-nt)
      "\\\\.\\COM2"
    "/dev/ttyS1")
  "The serial port for the RED side")

(defvar hft/black-port
  (if (eq system-type 'windows-nt)
      "\\\\.\\COM1"
    "/dev/ttyS0")
  "The serial port for the BLACK side")


(defun setup-radio-env ()
  (interactive))

; This just serves as notes basically, need to clean this up.
(defun hft/do-stuff ()
  (copy-face 'default 'hft-black-face)
  (copy-face 'default 'hft-red-face)
  (set-face-attribute 'hft-black-face nil
		      :background "#000000"
		      :foreground "#fffff0")
  (set-face-attribute 'hft-red-face nil
		      :background "#640000"
		      :foreground "#fffff0")

  (serial-term hft/red-port 115200)
  (serial-term hft/black-port 115200)

  (with-current-buffer hft/red-port
    (set (make-local-variable 'emulation-mode-map-alists) nil)
    (set (make-local-variable 'term-default-bg-color) "#640000")
    (set (make-local-variable 'term-default-fg-color) "#fffff0")
    (term-ansi-reset))

  (with-current-buffer hft/black-port
    (set (make-local-variable 'emulation-mode-map-alists) nil)
    (set (make-local-variable 'term-default-bg-color) "#000000")
    (set (make-local-variable 'term-default-fg-color) "#fffff0")
    (term-ansi-reset)))