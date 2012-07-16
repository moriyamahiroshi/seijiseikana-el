;;; seijiseikana.el --- Japanese Seiji and Seiakana utilities -*- coding: utf-8 -*-

;; Copyright (C) 2012  MORIYAMA Hiroshi

;; Author: MORIYAMA Hiroshi (@moriyama164) <hiroshi@kvd.biglobe.ne.jp>
;; Keywords: i18n

;; seijiseikana.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; seijiseikana.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'sgml-mode)

(defgroup seijiseikana nil
  "Seiji and Seikana utilities."
  :prefix 'seijiseikana-)

(defun seijiseikana-sgml-tag-name-is (tag-name tag)
  (if (and tag
           (or (eql (sgml-tag-type tag) 'open)
               (eql (sgml-tag-type tag) 'close)))
      (let ((normalized-tag-name (upcase (sgml-tag-name tag))))
        (if (string-equal normalized-tag-name tag-name)
            normalized-tag-name))))

(defun seijiseikana-point-inside-sgml-element-p (element-name)
  (let ((result nil)
        (element-name (upcase element-name)))
    (save-excursion
      (save-restriction
        (save-match-data
          (widen)
          (let (context)
            (while (setq context (sgml-get-context))
              (if (member element-name
                          (mapcar #'(lambda (tag)
                                      (seijiseikana-sgml-tag-name-is element-name
                                                                     tag))
                                  context))
                  (setq result context)))
            (if result t)))))))

(defun seijiseikana-upcase-kana-character (char)
  (cond
   ((or (= char ?っ)
	(and (= (% char 2) 1)
	     (or (and (>= char #x30E3) (<= char #x30E7))   ;ャュョ
		 (and (>= char #x30A1) (<= char #x30A9))   ;ァィゥェォ
		 (and (>= char #x3083) (<= char #x3087))   ;ゃゅょ
		 (and (>= char #x3041) (<= char #x3049)))) ;ぁぃぅぇぉ
	(= char ?ゎ))
    (1+ char))
   ((= char ?ヵ) ?カ)
   ((= char ?ヶ) ?ケ)
   ((= char ?ゕ) ?か)
   ((= char ?ゖ) ?け)
   ((and (>= char #x31F0) (<= char #x31FF))
    (cond ((= char ?ㇰ) ?ク) ((= char ?ㇱ) ?シ) ((= char ?ㇲ) ?ス)
	  ((= char ?ㇳ) ?ト) ((= char ?ㇴ) ?ヌ) ((= char ?ㇵ) ?ハ)
	  ((= char ?ㇶ) ?ヒ) ((= char ?ㇷ) ?フ) ((= char ?ㇸ) ?ヘ)
	  ((= char ?ㇹ) ?ホ) ((= char ?ㇺ) ?ム) ((= char ?ㇻ) ?ラ)
	  ((= char ?ㇼ) ?リ) ((= char ?ㇽ) ?ル) ((= char ?ㇾ) ?レ)
	  ((= char ?ㇿ) ?ロ)))
   (t
    char)))

(defvar seijiseikana-single-character-kanji-variants-alist
  '(("乘" . "乗")
    ("亂" . "乱")
    ("亞" . "亜")
    ("佛" . "仏")
    ("來" . "来")
    ("假" . "仮")
    ("傳" . "伝")
    ("僞" . "偽")
    ("價" . "価")
    ("儉" . "倹")
    ("儘" . "侭")
    ("兒" . "児")
    ("兩" . "両")
    ("剩" . "剰")
    ("劍" . "剣")
    ("劑" . "剤")
    ("勞" . "労")
    ("勵" . "励")
    ("勸" . "勧")
    ("區" . "区")
    ("卷" . "巻")
    ("參" . "参")
    ("單" . "単")
    ("嚴" . "厳")
    ("囑" . "嘱")
    ("圈" . "圏")
    ("國" . "国")
    ("圍" . "囲")
    ("圓" . "円")
    ("圖" . "図")
    ("團" . "団")
    ("墮" . "堕")
    ("壓" . "圧")
    ("壘" . "塁")
    ("壞" . "壊")
    ("壤" . "壌")
    ("壯" . "壮")
    ("壹" . "壱")
    ("壽" . "寿")
    ("奧" . "奥")
    ("奬" . "奨")
    ("孃" . "嬢")
    ("學" . "学")
    ("寢" . "寝")
    ("實" . "実")
    ("寫" . "写")
    ("寶" . "宝")
    ("將" . "将")
    ("專" . "専")
    ("對" . "対")
    ("屆" . "届")
    ("屬" . "属")
    ("峽" . "峡")
    ("嶽" . "岳")
    ("巖" . "巌")
    ("帶" . "帯")
    ("廢" . "廃")
    ("廣" . "広")
    ("廳" . "庁")
    ("彈" . "弾")
    ("徑" . "径")
    ("從" . "従")
    ("恆" . "恒")
    ("惠" . "恵")
    ("惡" . "悪")
    ("惱" . "悩")
    ("愼" . "慎")
    ("慘" . "惨")
    ("應" . "応")
    ("懷" . "懐")
    ("戀" . "恋")
    ("戰" . "戦")
    ("戲" . "戯")
    ("拂" . "払")
    ("拔" . "抜")
    ("拜" . "拝")
    ("挾" . "挟")
    ("插" . "挿")
    ("搖" . "揺")
    ("搜" . "捜")
    ("擇" . "択")
    ("擔" . "担")
    ("據" . "拠")
    ("擧" . "挙")
    ("擴" . "拡")
    ("攝" . "摂")
    ("收" . "収")
    ("效" . "効")
    ("敍" . "叙")
    ("敕" . "勅")
    ("數" . "数")
    ("斷" . "断")
    ("晝" . "昼")
    ("曉" . "暁")
    ("會" . "会")
    ("條" . "条")
    ("棧" . "桟")
    ("榮" . "栄")
    ("樂" . "楽")
    ("樓" . "楼")
    ("樞" . "枢")
    ("樣" . "様")
    ("檢" . "検")
    ("櫻" . "桜")
    ("權" . "権")
    ("歐" . "欧")
    ("歡" . "歓")
    ("歸" . "帰")
    ("殘" . "残")
    ("毆" . "殴")
    ("氣" . "気")
    ("沒" . "没")
    ("淨" . "浄")
    ("淺" . "浅")
    ("溪" . "渓")
    ("滯" . "滞")
    ("滿" . "満")
    ("潛" . "潜")
    ("澁" . "渋")
    ("澤" . "沢")
    ("濕" . "湿")
    ("濟" . "済")
    ("濱" . "浜")
    ("瀧" . "滝")
    ("灣" . "湾")
    ("燒" . "焼")
    ("營" . "営")
    ("爐" . "炉")
    ("爭" . "争")
    ("爲" . "為")
    ("犧" . "犠")
    ("狹" . "狭")
    ("獨" . "独")
    ("獵" . "猟")
    ("獸" . "獣")
    ("獻" . "献")
    ("畫" . "画")
    ("當" . "当")
    ("疊" . "畳")
    ("癡" . "痴")
    ("發" . "発")
    ("盜" . "盗")
    ("盡" . "尽")
    ("眞" . "真")
    ("碎" . "砕")
    ("祕" . "秘")
    ("祿" . "禄")
    ("禪" . "禅")
    ("禮" . "礼")
    ("稱" . "称")
    ("稻" . "稲")
    ("穩" . "穏")
    ("竊" . "窃")
    ("竝" . "並")
    ("粹" . "粋")
    ("絲" . "糸")
    ("經" . "経")
    ("縣" . "県")
    ("縱" . "縦")
    ("總" . "総")
    ("繩" . "縄")
    ("繪" . "絵")
    ("繼" . "継")
    ("續" . "続")
    ("纖" . "繊")
    ("聲" . "声")
    ("聽" . "聴")
    ("肅" . "粛")
    ("腦" . "脳")
    ("膽" . "胆")
    ("臟" . "臓")
    ("臺" . "台")
    ("與" . "与")
    ("舊" . "旧")
    ("莊" . "荘")
    ("萬" . "万")
    ("藏" . "蔵")
    ("藥" . "薬")
    ("藪" . "薮")
    ("處" . "処")
    ("號" . "号")
    ("螢" . "蛍")
    ("蟲" . "虫")
    ("蠶" . "蚕")
    ("蠻" . "蛮")
    ("衞" . "衛")
    ("裝" . "装")
    ("襃" . "褒")
    ("覺" . "覚")
    ("覽" . "覧")
    ("觀" . "観")
    ("觸" . "触")
    ("謠" . "謡")
    ("證" . "証")
    ("譯" . "訳")
    ("譽" . "誉")
    ("讀" . "読")
    ("變" . "変")
    ("讓" . "譲")
    ("豐" . "豊")
    ("貳" . "弐")
    ("賣" . "売")
    ("贊" . "賛")
    ("踐" . "践")
    ("輕" . "軽")
    ("轉" . "転")
    ("辭" . "辞")
    ("遙" . "遥")
    ("遞" . "逓")
    ("遲" . "遅")
    ("邊" . "辺")
    ("醉" . "酔")
    ("醫" . "医")
    ("釀" . "醸")
    ("釋" . "釈")
    ("錢" . "銭")
    ("鎭" . "鎮")
    ("鐵" . "鉄")
    ("鑄" . "鋳")
    ("鑛" . "鉱")
    ("關" . "関")
    ("附" . "付")
    ("陷" . "陥")
    ("隨" . "随")
    ("險" . "険")
    ("隱" . "隠")
    ("雙" . "双")
    ("雜" . "雑")
    ("霸" . "覇")
    ("靈" . "霊")
    ("靜" . "静")
    ("顯" . "顕")
    ("飮" . "飲")
    ("騷" . "騒")
    ("驅" . "駆")
    ("驗" . "験")
    ("驛" . "駅")
    ("髓" . "髄")
    ("體" . "体")
    ("髮" . "髪")
    ("鬪" . "闘")
    ("鷄" . "鶏")
    ("鹽" . "塩")
    ("麥" . "麦")
    ("默" . "黙")
    ("點" . "点")
    ("黨" . "党")
    ("齊" . "斉")
    ("齋" . "斎")
    ("齒" . "歯")
    ("齡" . "齢")
    ("龍" . "竜")
    ("龜" . "亀"))
  "單漢字の異體字の聯想リスト。CARが「正字」、CDRが「略字」。")

(defvar seijiseikana-context-dependent-kanji-variants-alist
  '(("一攫千金" . "一獲千金")
    ("下尅上" . "下克上")
    ("企劃" . "企画")
    ("倒潰" . "倒壊")
    ("全潰" . "全壊")
    ("制禦" . "制御")
    ("勘辨" . "勘弁")
    ("區劃" . "区画")
    ("半潰" . "半壊")
    ("叡才" . "英才")
    ("叡智" . "英知")
    ("崩潰" . "崩壊")
    ("恩誼" . "恩義")
    ("意嚮" . "意向")
    ("慰藉料" . "慰謝料")
    ("戰歿" . "戦没")
    ("按分" . "案分")
    ("暗誦" . "暗唱")
    ("根柢" . "根底")
    ("死歿" . "死没")
    ("歿前" . "没前")
    ("歿年" . "没年")
    ("歿後" . "没後")
    ("決潰" . "決壊")
    ("潰乱" . "壊乱")
    ("潰滅" . "壊滅")
    ("潰走" . "壊走")
    ("火焔" . "火炎")
    ("理窟" . "理屈")
    ("生歿" . "生没")
    ("申し訣" . "申し訳")               ;?
    ("病歿" . "病没")
    ("皆既蝕" . "皆既食")
    ("編輯" . "編集")
    ("缺か" . "欠か")
    ("缺か" . "欠こ")
    ("缺き" . "欠き")
    ("缺く" . "欠く")
    ("缺け" . "欠け")                   ;MEMO: 欠/缺/闕
    ("缺けば" . "欠けば")
    ("缺片" . "欠片")
    ("聯絡" . "連絡")
    ("脚註" . "脚注")
    ("苑地" . "園地")
    ("萎縮" . "委縮")
    ("言ひ訣" . "言ひ訳")               ;?
    ("言訣" . "言訳")                   ;?
    ("計劃" . "計画")
    ("劃策" . "画策")
    ("誡告" . "戒告")
    ("豫告" . "予告")
    ("豫報" . "予報")
    ("豫定" . "予定")
    ("豫測" . "予測")
    ("豫約" . "予約")
    ("辨當" . "弁当")
    ("遺蹟" . "遺跡")
    ("鑿岩機" . "削岩機")
    ("間歇泉" . "間欠泉")
    ("闇夜" . "暗夜")
    ("關聯" . "関連")
    ("防禦" . "防御")
    ("下付" . "下付")                   ;付・附
    ("交付" . "交付")                   ;付・附
    ("付囑" . "付嘱")                   ;付・附
    ("付梓" . "付梓")                   ;付・附
    ("付與" . "付与")                   ;付・附
    ("付託" . "付託")                   ;付・附
    ("付議" . "付議")                   ;付・附
    ("納付" . "納付")                   ;付・附
    ("給付" . "給付")                   ;付・附
    ("託付" . "託付")                   ;付・附
    ("送付" . "送付")                   ;付・附
    ("送り付" . "送り付")               ;付・附
    ("還付" . "還付")                   ;付・附
    ("配付" . "配付")                   ;付・附
    ("陣歿" . "陣没")
    ("颱風" . "台風")
    ("叮嚀" . "丁寧")
    ("餘地" . "余地")
    ("餘ら" . "余ら")
    ("餘り" . "余り")
    ("餘つ" . "余つ")
    ("餘っ" . "余っ")
    ("餘る" . "余る")
    ("餘れ" . "余れ")
    ("餘ろ" . "余ろ")
    ("餘白" . "余白")
    ("餘計" . "余計"))
  "「同音の漢字による書きかえ」などにより書き換へられた語のリスト。")

(defvar seijiseikana-not-converting-words-list
  ;; TODO: カスタマイズ變數にしたいが、更新されたらもちろん動作に即時反映
  ;; させなければならない。コマンドの實行時にほかのリストと連結するやうに
  ;; し、且つリストの先頭に追加すれば良いだらう、多分。現在は
  ;; `seijiseikana-seiji-ryakuji-alist' の定義時に連結し文字列の長い順に
  ;; ソートしてゐる。
  '("「国語改革」"
    "『国語改革』"
    "当用漢字"
    "現代仮名遣い")
  "變換しない語のリスト。主に固有名詞。")

(defvar seijiseikana-seiji-ryakuji-alist
  (sort (append seijiseikana-single-character-kanji-variants-alist
                seijiseikana-context-dependent-kanji-variants-alist
                (mapcar #'(lambda (s) (cons s s))
                        seijiseikana-not-converting-words-list))
        #'(lambda (a b) (> (length (car a)) (length (car b)))))
  "CAR部が「正字・正かなづかひ」の文字列、CDR部が「略字・現代仮名
遣い」の文字列の聯想リスト。CAR部の文字列が長い順に竝んでゐる。")

(defvar seijiseikana-ryakuji-seiji-alist
  (reverse (let ((alist seijiseikana-seiji-ryakuji-alist)
                 new-alist
                 pair)
             (while (setq pair (car alist))
               (setq new-alist (cons (cons (cdr pair)
                                           (car pair))
                                     new-alist))
               (setq alist (cdr alist)))
             new-alist))
  "CAR部が「略字・現代仮名遣い」の文字列、CDR部が「正字・正かなづ
かひ」の文字列の聯想リスト。CAR部の文字列が長い順に竝んでゐる。")

(defcustom seijiseikana-sgml-elements-do-not-convert
  '("blockquote" "q" "a" "cite" "em" "code" "kbd" "samp")
  "`seijiseikana-replace-string-by-alist' を `html-mode' 等で使用
したときに變換しない要素名のリスト。"
  :group 'seijiseikana
  :type 'list)

(defun seijiseikana-replace-string-by-alist (alist start end &optional sgml-elements-do-not-convert)
  "聯想配列 ALIST の内容に基いてリージョン内の文字列を置換する。

第一引數で與へられた聯想配列 ALIST の、car部の文字列を檢索し、同
cdr部の文字列に置換へる。

第二引數 START は變換對象とするリージョンの開始位置、第三引數 END
は同終了位置である。

省略可能な第四引數 SGML-ELEMENTS-DO-NOT-CONVERT が NIL であれば、
カスタマイズ變數 `seijiseikana-sgml-elements-do-not-convert' に含
まれる名前の要素の内容、及び屬性値を變換しないやうになる。同引数が
空ではないリストであれば、カスタマイズ變數
`seijiseikana-sgml-elements-do-not-convert' のかはりにこれを用ゐる。
リスト以外の非NILであれば、變換對象を制限しない。"
  (save-excursion
    (save-restriction
      (save-match-data
        (narrow-to-region start end)
        (goto-char (point-min))
        (let ((re (regexp-opt (mapcar 'car alist))))
          (while (re-search-forward re nil t)
            (if (not (member
                      nil
                      (mapcar
                       (lambda (element-name)
                         (not (seijiseikana-point-inside-sgml-element-p element-name)))
                       (or (and (listp sgml-elements-do-not-convert)
                                sgml-elements-do-not-convert)
                           seijiseikana-sgml-elements-do-not-convert))))
                (replace-match (cdr (assoc (match-string 0) alist))))))))))

(defun seijiseikana-ryakuji-region (start end)
  (interactive "r")
  "リージョン内の正字を略字に變換する。"
  (seijiseikana-replace-string-by-alist seijiseikana-seiji-ryakuji-alist
                                        start end))

(defun seijiseikana-seiji-region (start end)
  (interactive "r")
  "リージョン内の略字を正字に變換する。"
  (seijiseikana-replace-string-by-alist seijiseikana-ryakuji-seiji-alist
                                        start end))

(defun seijiseikana-upcase-kana-region (start end)
  "リージョン内の小書きの假名(ぁぃぅゃゅょっ…等)を通常の假名(あい
うやゆよつ…)へ變換する。"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (not (= (point) end))
        (let* ((orig-char (following-char))
               (char (seijiseikana-upcase-kana-character orig-char)))
	  (if (= char orig-char)
              (forward-char 1)
            (delete-char 1)
            (insert-char char 1)))))))

;;; Menu Commands

(require 'easymenu)

(defvar seijiseikana-menu-spec-en
  '("Seijiseikana"
    ["Replace from RYAKUJI to SEIJI Region"
     seijiseikana-seiji-region mark-active]
    ["Replace from SEIJI to RYAKUJI Region"
     seijiseikana-ryakuji-region mark-active]))

(defvar seijiseikana-menu-spec-ja
  '("Seijiseikana"
    ["リージョンの漢字を正字に變換"
     seijiseikana-seiji-region mark-active]
    ["リージョンの漢字を略字に變換"
     seijiseikana-ryakuji-region mark-active]))

(easy-menu-define seijiseikana-menu global-map "seijiseikana"
  (cond
   ((or (equal current-language-environment "Japanese")
        (string-match "\\`ja_JP[.@]" (or (getenv "LC_MESSAGES") "")))
    seijiseikana-menu-spec-ja)
   (t
    seijiseikana-menu-spec-en)))

(add-submenu nil seijiseikana-menu "Help")


(provide 'seijiseikana)
;;; seijiseikana.el ends here
