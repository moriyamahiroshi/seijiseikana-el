# Seijiseikana.el  <!-- -*- mode: markdown; coding: utf-8 -*- -->

とりあへず。

英語(函數名、變數名、その他)が怪しいのでツッコミ待ち。

## Usage:

Add the following code to your `~/.emacs.d/init.el` or `~/.emacs.el`:

    (add-to-list 'load-path "/PATH/TO/seijiseikana-el-DIRECTORY")
    (require 'seijiseikana)

### Commands:

  - M-x seijiseikana-seiji-region : リージョン内の「略字」を「正字」へ變換する。

  - M-x seijiseikana-ryakuji-region : リージョン内の「正字」を「略字」へ變換する。

## TODO:

  - 辭書に誤が無いかチェック。

  - 鈎括弧や引用符で括られてゐる一文字は變換しないやうにする。

  - `M-u` (upcase-word) で "ぁぃぅぇぉ" を "あいうえお" にする defacvice。

  - 「書きかえ」られた漢字の修復の充實。

  - 辭書を別ファイルにする?

  - 漢字だけでなく假名遣も變換(elispでの優先度は低)。

## Author:

  - MORIYAMA Hiroshi (@moriyama164) <hiroshi@kvd.biglobe.ne.jp>
