# pathkell
Haskell製のパストレーサを目指してます．

## Description
研究でパストレーサのFPGA実装を作っていて，基本的には今の所高位合成で作成しているが，Xilinx社の的供する高位合成ツール，Vivado HLSではC/C++の使用を強要されてしまう．
そこで，Haskellで書いてVerilogHDLにしてくれるやつを見つけたので，参照透過性を担保できるHaskellでパストレーサを書けばパイプライン化がとても楽なのではと思い，見切り発車してみたプロジェクト．

## Implementation
目標としては画像配列の管理にState Modadを使用し，それ以外では純粋関数のみ使用して実装したい．
多分一番の難関は乱数の生成部分．（これはHLSでもとても悩ましかった）

## Bibliography
- 参考にしたプログラム
    - https://github.com/githole/simple-pathtracer/blob/simple-pathtracer-norecursion/simplept.cpp
- 参考にしたサイト
    - http://e-tipsmemo.hatenablog.com/entry/2018/01/24/000000
    - https://qiita.com/mebiusbox2/items/89e2db3b24e4c39502fe

## License
BSD3
