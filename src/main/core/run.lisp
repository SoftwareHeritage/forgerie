(in-package #:forgerie-core)

(defun run (forge-for-import forge-for-export)
 (export-forge
  forge-for-export
  (import-forge
   forge-for-import)))
