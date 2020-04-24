;;; native-complete-test.el --- test for native complete -*- lexical-binding: t -*-

;;; Commentary:

;; Provides test for `ert'

;;; Code:

(require 'ert)

(require 'native-complete)

(defmacro native-complete-test-with-setup (cmd buffer-contents &rest body)
  `(cl-destructuring-bind (native-complete--common . native-complete--prefix)
       (native-complete--split-command ,cmd)
     (with-current-buffer (get-buffer-create native-complete--buffer)
       (erase-buffer)
       (insert ,buffer-contents))
     (let ((native-complete--command ,cmd))
       ,@body)))

(ert-deftest native-complete-test-tcsh ()
  (should (equal (native-complete-test-with-setup
                  "foo" "foobar y\n"
                  (native-complete--get-completions))
                 '("foobar")))
  (should (equal (native-complete-test-with-setup
                  "ls ~/env/"
                  "ls ~/env/
setup_foo      setup_bar* setup_baz*     
ls ~/env/setup_y
ubnx000723>"
                  (native-complete--get-completions))
                 '("setup_foo" "setup_bar" "setup_baz")))
  (should (equal (native-complete-test-with-setup
                  "git"
                  "git
git                     git-cvsserver           gitlab-runner           git-parse-remote        git-show
git-cvsexportcommit     git-instaweb            git-pack-redundant      git-shell               git-write-tree
git-cvsimport           gitk                    git-pack-refs           git-shortlog            
gity
ubnx000723>"
                  (native-complete--get-completions))
                 '("git" "git-cvsserver" "gitlab-runner"
                   "git-parse-remote" "git-show" "git-cvsexportcommit"
                   "git-instaweb" "git-pack-redundant" "git-shell"
                   "git-write-tree" "git-cvsimport" "gitk"
                   "git-pack-refs" "git-shortlog"))))

(ert-deftest native-complete-test-bash ()
  (should (equal (native-complete-test-with-setup
                  "foo"
                  "foobar yCCCCK

foobaz"
                  (native-complete--get-completions))
                 '("foobar")))
  (should (equal (native-complete-test-with-setup
                  "ls ~/env/"
                  "ls ~/env/
setup_foo      setup_bar  setup_baz      

sodf2093> 
ls ~/env/setup_y"
                  (native-complete--get-completions))
                 '("setup_foo" "setup_bar" "setup_baz")))
  (should (equal (native-complete-test-with-setup
                  "git"
                  "git
Display all 146 possibilities? (y or n)
git                      git-cvsserver            gitlab-runner            git-patch-id             git-show-ref
git-cvsimport            gitk                     git-parse-remote         git-show-index           

sodf2093> 
git"
                  (native-complete--get-completions))
                 '("git" "git-cvsserver" "gitlab-runner"
                   "git-patch-id" "git-show-ref" "git-cvsimport"
                   "gitk" "git-parse-remote" "git-show-index")))
  (should (equal (native-complete-test-with-setup
                  "di"
                  "di
dialog            diff-jars     dijkstra          directfb-csource  dirs              disco           disown
diff3             dig           directfb-config   dirname           disable-paste     dislocate       

sodf2093> 
diyK
"
                  (native-complete--get-completions))
                 '("dialog" "diff-jars" "dijkstra"
                   "directfb-csource" "dirs" "disco"
                   "disown" "diff3" "dig"
                   "directfb-config" "dirname" "disable-paste"
                   "dislocate"))))

(ert-deftest native-complete-test-cad ()
  (should (equal (native-complete-test-with-setup
                  "get_"
                  "get_
get_attribute_list                get_attribute_option              get_attribute_value          
get_tool_option                   get_transcript_tool               get_xfdb_output_directory    
get_xset_handling               
KSETER> get_yK"
                  (native-complete--get-completions))
                 '("get_attribute_list" "get_attribute_option" "get_attribute_value"
                   "get_tool_option" "get_transcript_tool" "get_xfdb_output_directory"
                   "get_xset_handling")))
  (should (equal (native-complete-test-with-setup
                  "ls ~/env/"
                  "ls ~/env/
Display all results? [y/n]
~/env/setup_foo     ~/env/setup_bar ~/env/setup_baz     

KSETER> ls ~/setup_ySETER> K"
                  (native-complete--get-completions))
                 '("setup_foo" "setup_bar" "setup_baz"))))

(provide 'native-complete-test)

;;; native-complete-test.el ends here
