これは何
========
xyzzy の `setf` で多値を扱えるようにする。

インストール
============

NetInstaller から
-----------------
<del>[カフェイン中毒] からどうぞ。</del>

  [カフェイン中毒]: http://bowbow99.sakura.ne.jp/xyzzy/packages.l


使い方
======
読み込むだけで多値を受け取る汎変数に `setf` できるようになります。

    (eval-when (:execute :compile-toplevel :load-toplevel)
      (require "setf-values"))


注意点、既知の問題など
======================
- 以下の関数を上書きしています。
  - `lisp::optimize-setf-method`
  - `lisp::setf-expand-1`
  - `lisp::get-setf-method`

バグ報告、質問、要望などは [GitHubIssues] か [@bowbow99] あたりへお願いします。

  [GitHubIssues]: http://github.com/bowbow99/xyzzy.setf-values/issues
  [@bowbow99]: http://twitter.com/bowbow99
