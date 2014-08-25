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
