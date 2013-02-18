
(setq user-emacs-directory "/home/luebsj/emacs/")
(setq jkl/pkg-path (concat user-emacs-directory "pkg/"))
(setq jkl/info-path (concat user-emacs-directory "info/"))

(eval-after-load "slime"
  '(jkl/custom-set 'inferior-lisp-program "sbcl"))

(eval-after-load "jde"
  '(progn
     (jkl/custom-set 'jde-jdk-registry
                     '(("1.7.0" . "/opt/java")))))

(set-default emms-player-mpd-connect-function 'jkl/mpd-local-connect)

(eval-after-load "emms"
  '(progn
     (require 'emms-player-mpd)

     (defun jkl/mpd-local-connect (name buffer host service &rest parameters)
       (when parameters (error "Don't know what to do with mpd-connect parameters!"))
       (make-network-process :name name :buffer buffer
                             :family 'local :service service))
     (jkl/custom-set 'emms-player-list (cons 'emms-player-mpd emms-player-list))
     (jkl/custom-set 'emms-player-mpd-connect-function 'jkl/mpd-local-connect)
     (jkl/custom-set 'emms-volume-change-function 'emms-volume-mpd-change)
     (jkl/custom-set 'emms-player-mpd-music-directory "~/music")

     (jkl/custom-set 'emms-player-mpd-supported-regexp
                     "\\`http://\\|\\.\\([Mm][Pp]3\\|[Oo][Gg][Gg]\\|[Oo][Gg][Aa]\\|[Oo][Gg][Gg]\\|[Oo][Gg][Aa]\\|[Ff][Ll][Aa][Cc]\\|[Oo][Pp][Uu][Ss]\\|[Oo][Gg][Gg]\\|[Oo][Gg][Aa]\\|[Ww][Aa][Vv]\\|[Aa][Ii][Ff][Ff]\\|[Aa][Ii][Ff]\\|[Aa][Uu]\\|[Ss][Nn][Dd]\\|[Pp][Aa][Ff]\\|[Ii][Ff][Ff]\\|[Ss][Vv][Xx]\\|[Ss][Ff]\\|[Vv][Oo][Cc]\\|[Ww]64\\|[Pp][Vv][Ff]\\|[Xx][Ii]\\|[Hh][Tt][Kk]\\|[Cc][Aa][Ff]\\|[Ss][Dd]2\\|[Ww][Aa][Vv]\\|[Aa][Uu]\\|[Aa][Ii][Ff][Ff]\\|[Aa][Ii][Ff]\\|[Dd][Ff][Ff]\\|[Dd][Ss][Ff]\\|[Aa][Aa][Cc]\\|[Ww][Vv]\\|669\\|[Aa][Mm][Ff]\\|[Aa][Mm][Ss]\\|[Dd][Bb][Mm]\\|[Dd][Ff][Mm]\\|[Dd][Ss][Mm]\\|[Ff][Aa][Rr]\\|[Ii][Tt]\\|[Mm][Ee][Dd]\\|[Mm][Dd][Ll]\\|[Mm][Oo][Dd]\\|[Mm][Tt][Mm]\\|[Mm][Tt]2\\|[Oo][Kk][Tt]\\|[Ss]3[Mm]\\|[Ss][Tt][Mm]\\|[Uu][Ll][Tt]\\|[Uu][Mm][Xx]\\|[Xx][Mm]\\|16[Ss][Vv]\\|3[Gg]2\\|3[Gg][Pp]\\|4[Xx][Mm]\\|8[Ss][Vv][Xx]\\|[Aa][Aa]3\\|[Aa][Aa][Cc]\\|[Aa][Cc]3\\|[Aa][Ff][Cc]\\|[Aa][Ii][Ff]\\|[Aa][Ii][Ff][Cc]\\|[Aa][Ii][Ff][Ff]\\|[Aa][Ll]\\|[Aa][Ll][Aa][Ww]\\|[Aa][Mm][Rr]\\|[Aa][Nn][Ii][Mm]\\|[Aa][Pp][Cc]\\|[Aa][Pp][Ee]\\|[Aa][Ss][Ff]\\|[Aa][Tt][Rr][Aa][Cc]\\|[Aa][Uu]\\|[Aa][Uu][Dd]\\|[Aa][Vv][Ii]\\|[Aa][Vv][Mm]2\\|[Aa][Vv][Ss]\\|[Bb][Aa][Pp]\\|[Bb][Ff][Ii]\\|[Cc]93\\|[Cc][Aa][Kk]\\|[Cc][Ii][Nn]\\|[Cc][Mm][Vv]\\|[Cc][Pp][Kk]\\|[Dd][Aa][Uu][Dd]\\|[Dd][Cc][Tt]\\|[Dd][Ii][Vv][Xx]\\|[Dd][Tt][Ss]\\|[Dd][Vv]\\|[Dd][Vv][Dd]\\|[Dd][Xx][Aa]\\|[Ee][Aa][Cc]3\\|[Ff][Ii][Ll][Mm]\\|[Ff][Ll][Aa][Cc]\\|[Ff][Ll][Cc]\\|[Ff][Ll][Ii]\\|[Ff][Ll][Ll]\\|[Ff][Ll][Xx]\\|[Ff][Ll][Vv]\\|[Gg]726\\|[Gg][Ss][Mm]\\|[Gg][Xx][Ff]\\|[Ii][Ss][Ss]\\|[Mm]1[Vv]\\|[Mm]2[Vv]\\|[Mm]2[Tt]\\|[Mm]2[Tt][Ss]\\|[Mm]4[Aa]\\|[Mm]4[Bb]\\|[Mm]4[Vv]\\|[Mm][Aa][Dd]\\|[Mm][Jj]2\\|[Mm][Jj][Pp][Ee][Gg]\\|[Mm][Jj][Pp][Gg]\\|[Mm][Kk][Aa]\\|[Mm][Kk][Vv]\\|[Mm][Ll][Pp]\\|[Mm][Mm]\\|[Mm][Mm][Ff]\\|[Mm][Oo][Vv]\\|[Mm][Pp]+\\|[Mm][Pp]1\\|[Mm][Pp]2\\|[Mm][Pp]3\\|[Mm][Pp]4\\|[Mm][Pp][Cc]\\|[Mm][Pp][Ee][Gg]\\|[Mm][Pp][Gg]\\|[Mm][Pp][Gg][Aa]\\|[Mm][Pp][Pp]\\|[Mm][Pp][Uu]\\|[Mm][Vv][Ee]\\|[Mm][Vv][Ii]\\|[Mm][Xx][Ff]\\|[Nn][Cc]\\|[Nn][Ss][Vv]\\|[Nn][Uu][Tt]\\|[Nn][Uu][Vv]\\|[Oo][Gg][Aa]\\|[Oo][Gg][Mm]\\|[Oo][Gg][Vv]\\|[Oo][Gg][Xx]\\|[Oo][Mm][Aa]\\|[Oo][Gg][Gg]\\|[Oo][Mm][Gg]\\|[Pp][Ss][Pp]\\|[Pp][Vv][Aa]\\|[Qq][Cc][Pp]\\|[Qq][Tt]\\|[Rr]3[Dd]\\|[Rr][Aa]\\|[Rr][Aa][Mm]\\|[Rr][Ll]2\\|[Rr][Mm]\\|[Rr][Mm][Vv][Bb]\\|[Rr][Oo][Qq]\\|[Rr][Pp][Ll]\\|[Rr][Vv][Cc]\\|[Ss][Hh][Nn]\\|[Ss][Mm][Kk]\\|[Ss][Nn][Dd]\\|[Ss][Oo][Ll]\\|[Ss][Oo][Nn]\\|[Ss][Pp][Xx]\\|[Ss][Tt][Rr]\\|[Ss][Ww][Ff]\\|[Tt][Gg][Ii]\\|[Tt][Gg][Qq]\\|[Tt][Gg][Vv]\\|[Tt][Hh][Pp]\\|[Tt][Ss]\\|[Tt][Ss][Pp]\\|[Tt][Tt][Aa]\\|[Xx][Aa]\\|[Xx][Vv][Ii][Dd]\\|[Uu][Vv]\\|[Uu][Vv]2\\|[Vv][Bb]\\|[Vv][Ii][Dd]\\|[Vv][Oo][Bb]\\|[Vv][Oo][Cc]\\|[Vv][Pp]6\\|[Vv][Mm][Dd]\\|[Ww][Aa][Vv]\\|[Ww][Ee][Bb][Mm]\\|[Ww][Mm][Aa]\\|[Ww][Mm][Vv]\\|[Ww][Ss][Aa][Uu][Dd]\\|[Ww][Ss][Vv][Gg][Aa]\\|[Ww][Vv]\\|[Ww][Vv][Ee]\\|[Aa][Yy]\\|[Gg][Bb][Ss]\\|[Gg][Yy][Mm]\\|[Hh][Ee][Ss]\\|[Kk][Ss][Ss]\\|[Nn][Ss][Ff]\\|[Nn][Ss][Ff][Ee]\\|[Ss][Aa][Pp]\\|[Ss][Pp][Cc]\\|[Vv][Gg][Mm]\\|[Vv][Gg][Zz]\\)\\'")
     (setq emms-player-mpd-server-name nil)
     (setq emms-player-mpd-server-port "/home/luebsj/.mpd/socket")))
