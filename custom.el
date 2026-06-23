;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values
   '((tp--master-file
      . "/Users/andersskibsted/pCloud Drive/Datalogi/Tidligere fag/LinAlgDat/EksamenNoter/Saetninger.typ")
     (eval with-eval-after-load 'dap-mode
      (dap-register-debug-template "LLDB::Run Fauxgrep MT"
                                   (list :type "lldb-vscode" :cwd
                                         "${workspaceFolder}" :request "launch"
                                         :program
                                         "${workspaceFolder}/fauxgrep-mt" :name
                                         "LLDB::Run Fauxgrep MT" :args
                                         '("-n" "5" "for" "test"))))))
 '(safe-local-variable-values
   '((eval with-eval-after-load 'dap-mode
      (dap-register-debug-template "LLDB::Run Peer"
       (list :type "lldb-vscode" :cwd "${workspaceFolder}" :request "launch"
             :program "${workspaceFolder}/peer.c" :name "LLDB::Run Fauxgrep MT"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
