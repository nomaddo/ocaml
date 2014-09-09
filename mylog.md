## 2014年  9月 10日 水曜日 00:04:41 JST
ブートストラップ成功した
そのためにrename_ident.mlを本来例外を投げて死ぬべきところを変更してある(234行目)
むなしい

## 2014年  9月  9日 火曜日 21:00:00 JST
make opt.optのときだけインターフェイスを作りなおすのは無理だという結論
ocamlcを改造しようとしてるがrename_ident.mlまわりのバグに出会う
モウダメ


## 2014年  8月 30日 土曜日 09:30:05 JST
[http://qiita.com/xtetsuji/items/555a1ef19ed21ee42873](upstreamの設定)

に書いてあるとおりに、upstreamを設定し本家と追従してみる
今ある4.02のコードは3ヶ月前までの内容
本家に追従したら今起きてるエラー解決しないかなーとか思ったけど甘かった
git mergeしてコンパイル通そうとしたら別のエラーでコンパイルできない
FormatまわりでcamlinternalFormatBasicとか読めないので諦める
コンパイル改造とかめんどくさすぎてやめたい
4.02にはバグもあるだろうし本家に追従したほうがいいんだろうけど

一時的にmergeしたものをgit stashして保管しておこうとしたけど
git stash popするときにUntruckなファイルが変更されてると
popできないらしくgit merge "stash@{1}"とか書くことに成る

git生産性悪いんじゃないかと思い始める、githubにおいて
オープンな開発するわけじゃあるまいし

## 2014年  8月 26日 火曜日 23:43:31 JST
型の展開は何とかなったので、次の問題をとく
コード生成部分をリンクするときにmake inconsistent assumptionをおこす

boot/ocamlrun ./ocamlopt -nostdlib -I stdlib -I otherlibs/dynlink -g -strict-sequence -w +33..39+48 -warn-error A-26 -bin-annot -safe-string -I utils -I parsing -I typing -I bytecomp -I asmcomp -I driver -I toplevel -I tools -c asmcomp/mach.ml
File "asmcomp/mach.ml", line 1:
Error: The files asmcomp/arch.cmi and asmcomp/mach.cmi
       make inconsistent assumptions over interface Arch

しらべたことまとめ
* origin/4.02とdiffをとってみるけどコード生成部分のコードは全く一緒である
* mach.cmiとarch.cmiをrmしてからコンパイルすると通るがリンク時にcmoが不整合と言われる
* http://caml.inria.fr/mantis/view.php?id=6344 に同様の問題があるが解決法が書いてない！

## 2014年  8月 18日 月曜日 21:27:16 JST
* 型の展開、full_expandは恐らく最初の型のデータ構造しか見てないため
(Head系の展開関数しか呼んでない)展開できてなかった模様
ぜんぜんfull_expanではない、なんじゃこれ。
* Env.tの型隠蔽がなくなるようにmliを編集

## 2014年  8月 16日 土曜日 17:50:13 JST
* rename_ident.ml のUnifyを変更、前言っていたAssertionでは落ちなくなった
* 別のAssertionに失敗する、Unify.unify_typexprのTconstrのときのパターン
問題は型の展開が完全でないせい。あらたに追加した/test_file/alias.mlでもAssertで
落ちる。Ctype.full_expandではうまく展開できていない？


## 2014年  7月 31日 木曜日 02:26:54 JST

* rename_ident.mlを作成
* [PR3] test/whole_test.mlで、ファイル内で定義した関数の名前の書き換えがうまく行かない
* [PR4] GADTで恐らくエラーを履いたため、rename_identないでunifyが失敗したことを表す例外を定義
  これは本当に機能しているのか…？　本来見逃してはいけない問題を見逃してはいないか
* リファクタリングの必要性。そろそろ汚くなってきて辛い
* get_context, access_tableなど似たようなことをするのだけど違う関数、みたいなのがいっぱい現れた


## 2014年  7月 18日 金曜日 15:00:31 JST
型の展開を考えないとー。
type_kind =
| Type_abstract  // type alias
| Type_record    // record.
| Type_variant   // type variance, A | B ... みたいなやつ
| Type_open      // いつ出てくるかわからない


## 2014年  7月 16日 水曜日 15:53:12 JST

* デフォルトで複製前のコードと複製後のコードのプリントをオフにした
  プリントするときは-mydumpをつけてコンパイルする
* [PR2] 7/14 のfunctorに対する処理を追加
* [PR1] let pat = exp でpatがvarでない場合に型がおかしくなるのを修正
* dupfun_devとdupfunをマージした

## 2014年  7月 14日 月曜日 16:50:21 JST
[PR1]型がヘン、patternが変数単体でないときには別個にテーブルへの登録をしないといけない

[PR2]functorに対する処理を何も書いてない
ファンクタの中を見るように処理を変更する
