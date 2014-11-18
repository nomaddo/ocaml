## 2014年 11月 17日 月曜日 17:58:29 JST
コンパイル時間・コンパイル後のコード増加は
Janeの人的には問題にならないらしい

## 2014年 11月 12日 水曜日 20:27:52 JST
real	13m12.323s
今日やったら改造コンパイラのコンパイルタイムが変わっていた

## 2014年 11月 10日 月曜日 17:40:03 JST
stdlibのコンパイル時間
real	0m5.055s
real	0m48.763s

## 2014年 11月  6日 木曜日 22:38:15 JST
typevar6 user 15m29.542s
original user 2m42.115s

## 2014年 11月  5日 水曜日 23:50:55 JST
too bigなかんすたち

- pervasives.ml

DEBUG: too big: 6
string_of_format 1322
type: ([V[5031] , V[5032] , V[5033] , V[5034] , V[5035] , V[5036]] format6 -> [] string)

DEBUG: too big: 8
^^ 1326
type: ([V[5128] , V[5129] , V[5130] , V[5131] , V[5148] , V[5147]] format6 -> ([V[5147] , V[5129] , V[5130] , V[5148] , V[5132] , V[5133]] format6 -> [V[5128] , V[5129] , V[5130] , V[5131] , V[5132] , V[5133]] format6))

- camlinternalFormat.ml

DEBUG: too big: 6
bprint_fmt 1338
type: ([] buffer -> ([V[17642] , V[17643] , V[17644] , V[17645] , V[17646] , V[17647]] CamlinternalFormatBasics.fmt -> [] unit))

DEBUG: too big: 6
string_of_fmt 2353
type: ([V[17676] , V[17677] , V[17678] , V[17679] , V[17680] , V[17681]] CamlinternalFormatBasics.fmt -> [] string)

DEBUG: too big: 12
string_of_fmtty 60253
type: ([V[98422] , V[98423] , V[98424] , V[98425] , V[98426] , V[98427] , V[98428] , V[98429] , V[98430] , V[98431] , V[98432] , V[98433]] CamlinternalFormatBasics.fmtty_rel -> [] string)

DEBUG: too big: 7
make_padding_fmt_ebb 60731
type: Tpoly (([Tunivar (Some(x)) , Tunivar (Some(y))] CamlinternalFormatBasics.padding -> ([V[115947] , V[115969] , V[115970] , V[115950] , V[115971] , V[115972]] CamlinternalFormatBasics.fmt -> [V[115968] , V[115969] , V[115970] , V[115971] , V[115972]] padding_fmt_ebb)), [Tunivar (Some(x)) ; Tunivar (Some(y))])

DEBUG: too big: 7
make_precision_fmt_ebb 60741
type: Tpoly (([Tunivar (Some(x)) , Tunivar (Some(y))] CamlinternalFormatBasics.precision -> ([V[116299] , V[116321] , V[116322] , V[116302] , V[116323] , V[116324]] CamlinternalFormatBasics.fmt -> [V[116320] , V[116321] , V[116322] , V[116323] , V[116324]] precision_fmt_ebb)), [Tunivar (Some(x)) ; Tunivar (Some(y))])

DEBUG: too big: 7
make_padprec_fmt_ebb 60749
type: Tpoly (([Tunivar (Some(x)) , Tunivar (Some(y))] CamlinternalFormatBasics.padding -> ([Tunivar (Some(z)) , Tunivar (Some(t))] CamlinternalFormatBasics.precision -> ([V[116646] , V[116668] , V[116669] , V[116649] , V[116670] , V[116671]] CamlinternalFormatBasics.fmt -> [V[116667] , V[116668] , V[116669] , V[116670] , V[116671]] padprec_fmt_ebb))), [Tunivar (Some(x)) ; Tunivar (Some(y)) ; Tunivar (Some(z)) ; Tunivar (Some(t))])

- scanf.ml

DEBUG: too big: 7
bscanf_format 4697
type: ([] Scanning.in_channel -> ([V(a[28702]) , V(b[28704]) , V(c[28706]) , V(d[28708]) , V(e[28710]) , V(f[28712])] format6 -> (([V(a[28702]) , V(b[28704]) , V(c[28706]) , V(d[28708]) , V(e[28710]) , V(f[28712])] format6 -> V(g[28714])) -> V(g[28714]))))

DEBUG: too big: 7
sscanf_format 4704
type: ([] string -> ([V(a[29009]) , V(b[29010]) , V(c[29011]) , V(d[29012]) , V(e[29013]) , V(f[29014])] format6 -> (([V(a[29009]) , V(b[29010]) , V(c[29011]) , V(d[29012]) , V(e[29013]) , V(f[29014])] format6 -> V(g[29006])) -> V(g[29006]))))

DEBUG: too big: 6
format_from_string 4714
type: ([] string -> ([V[29312] , V[29313] , V[29314] , V[29315] , V[29316] , V[29317]] format6 -> [V[29312] , V[29313] , V[29314] , V[29315] , V[29316] , V[29317]] format6))

DEBUG: too big: 6
format_of_string_fmtty 73282
type: ([] string -> ([V[136200] , V[136201] , V[136202] , V[136203] , V[136204] , V[136205]] CamlinternalFormatBasics.fmtty -> [V[136200] , V[136201] , V[136202] , V[136203] , V[136204] , V[136205]] CamlinternalFormatBasics.format6))

DEBUG: too big: 6
format_of_string_format 73288
type: ([] string -> ([V[136438] , V[136439] , V[136440] , V[136441] , V[136442] , V[136443]] CamlinternalFormatBasics.format6 -> [V[136438] , V[136439] , V[136440] , V[136441] , V[136442] , V[136443]] CamlinternalFormatBasics.format6))

- camlinternalOO.ml
DEBUG: too big: 5
dummy_class 1259
type: (([] string * [] int * [] int) -> (V[7578] * (V[7594] -> V[7593]) * (V[7597] -> V[7596]) * [] Obj.t))

- parsing/ast_mapper.ml

DEBUG: too big: 6
map_tuple3 1232
type: ((V[5241] -> V[5236]) -> ((V[5246] -> V[5237]) -> ((V[5251] -> V[5238]) -> ((V[5241] * V[5246] * V[5251]) -> (V[5236] * V[5237] * V[5238])))))

- typing/env.ml

DEBUG: too big: 5
find_all_simple_list 2498
type: (([] t -> [(V[31585] * V[31570])] Ident.tbl) -> (([] structure_components -> [V[31673] , [(V[31585] * V[31692])] list] Tbl.t) -> ((V[31585] -> (V[31549] -> V[31549])) -> ([[] Longident.t] option -> ([] t -> (V[31549] -> V[31549]))))))

- typing/typetexp.ml

DEBUG: too big: 5
spellcheck 1673
type: ([] Format.formatter -> ((([] string -> (V[24574] -> (V[24579] -> (([[] string] list * [] int) -> ([[] string] list * [] int))))) -> ([[] Longident.t] option -> (V[24532] -> (([V[24543]] list * [] int) -> ([[] string] list * V[24540]))))) -> (V[24532] -> ([] Longident.t -> [] unit))))

- typing/typeclass.ml

DEBUG: too big: 5
final_decl 2099
type: ([] Env.t -> ([] bool -> (([V[31604]] Parsetree.class_infos * [] Ident.t * [] Types.class_declaration * [] Ident.t * [] Types.class_type_declaration * [] Ident.t * [] Types.type_declaration * [] Ident.t * [] Types.type_declaration * [([] Typedtree.core_type * [] Asttypes.variance)] list * V[31529] * V[31530] * V[31531] * V[31542]) -> ([] Ident.t * [[] string] Asttypes.loc * [] Types.class_declaration * [] Ident.t * [] Types.class_type_declaration * [] Ident.t * [] Types.type_declaration * [] Ident.t * [] Types.type_declaration * V[31529] * V[31530] * V[31531] * V[31542] * [V[31542]] Typedtree.class_infos))))

DEBUG: too big: 14
extract_type_decls 2121
type: ((V[31644] * V[31645] * V[31669] * V[31647] * V[31670] * V[31666] * V[31667] * V[31651] * V[31668] * V[31653] * V[31654] * V[31655] * V[31656] * V[31671]) -> ([(V[31666] * V[31667] * V[31668] * V[31669] * V[31670] * V[31671])] list -> [(V[31666] * V[31667] * V[31668] * V[31669] * V[31670] * V[31671])] list))

DEBUG: too big: 18
merge_type_decls 2137
type: ((V[31727] * V[31728] * V[31701] * V[31730] * V[31703] * V[31732] * V[31705] * V[31734] * V[31707] * V[31736] * V[31737] * V[31738] * V[31739] * V[31740]) -> ((V[31733] * V[31735] * V[31729] * V[31731]) -> (V[31727] * V[31728] * V[31729] * V[31730] * V[31731] * V[31732] * V[31733] * V[31734] * V[31735] * V[31736] * V[31737] * V[31738] * V[31739] * V[31740])))

DEBUG: too big: 6
final_env 2156
type: ([] bool -> ([] Env.t -> (([] Ident.t * V[31779] * [] Types.class_declaration * [] Ident.t * [] Types.class_type_declaration * [] Ident.t * [] Types.type_declaration * [] Ident.t * [] Types.type_declaration * V[31787] * V[31788] * V[31789] * V[31790] * V[31791]) -> [] Env.t)))

DEBUG: too big: 11
check_coercions 2173
type: ([] Env.t -> ((V[32285] * V[32286] * V[32287] * V[32288] * V[32289] * V[32290] * [] Types.type_declaration * V[32292] * [] Types.type_declaration * V[32294] * V[32295] * [[] Location.t] list * V[32031] * V[32296]) -> (V[32285] * V[32286] * V[32287] * V[32288] * V[32289] * V[32290] * [] Types.type_declaration * V[32292] * [] Types.type_declaration * V[32294] * V[32295] * V[32296])))

- bytecomp/bytegen.ml
DEBUG: too big: 6
comp_string_switch 1440
type: (V[16268] -> (V[16270] -> (V[16272] -> (V[16274] -> (V[16276] -> (V[16278] -> [] unit))))))

- utils/misc.ml

DEBUG: too big: 4
fst4 1205
type: ((V[5091] * V[5101] * V[5102] * V[5103]) -> V[5091])

DEBUG: too big: 4
snd4 1207
type: ((V[5117] * V[5108] * V[5119] * V[5120]) -> V[5108])

DEBUG: too big: 4
thd4 1209
type: ((V[5134] * V[5135] * V[5125] * V[5137]) -> V[5125])

DEBUG: too big: 4
for4 1211
type: ((V[5151] * V[5152] * V[5153] * V[5142]) -> V[5142])

- parsing/ast_mapper.ml
DEBUG: too big: 4
map_tuple 1227
type: ((V[5192] -> V[5188]) -> ((V[5197] -> V[5189]) -> ((V[5192] * V[5197]) -> (V[5188] * V[5189]))))

DEBUG: too big: 6
map_tuple3 1232
type: ((V[5241] -> V[5236]) -> ((V[5246] -> V[5237]) -> ((V[5251] -> V[5238]) -> ((V[5241] * V[5246] * V[5251]) -> (V[5236] * V[5237] * V[5238])))))

- typing/typecore.ml
DEBUG: too big: 4
sort_pattern_variables 1506
type: ([([] Ident.t * V[13772] * V[13773] * V[13774] * V[13775])] list -> [([] Ident.t * V[13772] * V[13773] * V[13774] * V[13775])] list)



## 2014年 11月  5日 水曜日 10:20:05 JST
型変数の上限が6の時、多少の困難がある
- 多相関数の型変数の上限が6でも、型変数の上限6の関数の中に、更に型変数を4つ関数があり3^10乗をしないといけない、みたいなシチュエーションがある

そのため、
ctype.mlのassociate_fieldsにannotationをつけて多相性を消す

-rwxrwxr-x 1 tokuda tokuda 17686812 11月  5 11:24 ./ocamlopt.opt*

## 2014年 11月  4日 火曜日 13:48:50 JST
型変数上限が3の時
real 3m44.731s
user 5m29.762s
sys  0m22.962s

make world.opt -j 2

-rwxrwxr-x 1 tokuda tokuda 9676087 11月  4 13:47 ocamlopt.opt*

originalのほうは
-rwxrwxr-x 1 tokuda tokuda 5712348 11月  5 14:23 ocamlopt.opt*

大体1.7倍になった

## 2014年 10月  8日 水曜日 20:55:01 JST
lex.ml, compact.ml

## 2014年  9月 30日 火曜日 21:01:26 JST
An analysis of ocamlbuild/ocaml_compiler.ml
It seems that the env does not have definition of Pathname.t
[evidence/20140930/env.types.txt](log)
Just search by "occur" in below text file by "Pathname"

Of course, pathname.cmi import pathname.t and indicate it as string.

It could be good the compiler source table includes ocamlbuild and ocamldoc.
It is real product using usual OCaml's features.

## 2014年  9月 14日 日曜日 12:48:01 JST
too bigな関数たち、format系以外
型変数の上限、4くらいにはしたい

util/misc.ml fst4
ocamlbuild/ocaml_compiler.ml link_from_file
parsing/asp_mapper.ml map_tuple, map_tuple3

## 2014年  9月 13日 土曜日 20:24:26 JST
../boot/ocamlrun ../ocamlopt -strict-sequence -w +33..39 -g -warn-error A-3-26-32 -bin-annot -nostdlib -safe-string `./Compflags camlinternalFormat.cmx` -c camlinternalFormat.ml

## 2014年  9月 13日 土曜日 04:04:40 JST
今まで見てなかった例外が起きる原因
* unifyのTconstrのassertionが失敗する
ocamlbuildのPathname.t と stringのunifyとか
Pathname.tはmliファイルでシグニチャをincludeしており、
その中にtype t = stringと書いてある
なんでfull_expand_typeで展開してないのか

* Path.headの例外
IDがないとshadowingしていたりすると、一意に決まらない？

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
