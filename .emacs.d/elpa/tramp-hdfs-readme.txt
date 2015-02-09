Access hdfs over Tramp.
This program uses ssh to login to another machine that has hdfs client to access hdfs.
It then fires hdfs commands to do ls or fetch files.

Configuration:
  Add the following lines to your .emacs:

  (add-to-list 'load-path "<<directory containing tramp-hdfs.el>>")
  (require 'tramp-hdfs);;; Code:

Usage:
  open /hdfs:root@node-1:tmp/ in emacs
  where root   is the user that you want to use for ssh
        node-1 is the name of the machine that has hdfs client
