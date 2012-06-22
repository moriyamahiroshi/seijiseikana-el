# Seijiseikana.el

とりあへず。

英語(函數名、變數名、コミットログ、その他)が怪しいのでツッコミ待ち。

## Usage

Add the following code to your `~/.emacs.d/init.el` or `~/.emacs.el`:

    (add-to-list 'load-path "/PATH/TO/seijiseikana-el-DIRECTORY")
    (require 'seijiseikana)

### Commands

  - `M-x seijiseikana-seiji-region`: リージョン内の「略字」を「正字」へ變換する。

  - `M-x seijiseikana-ryakuji-region`: リージョン内の「正字」を「略字」へ變換する。

### Tutorial

畫像附きの分り易い解説を@nakinorさんに書いていただきました。ありがたうございます。

  - [正字(旧字体)と親しむスクリプトと Emacs の紹介]( http://sci.hateblo.jp/entry/usage_seijiseikana_system )

## Author

  - MORIYAMA Hiroshi (@moriyama164) <hiroshi@kvd.biglobe.ne.jp>
