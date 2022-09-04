# SVG Pathコマンド変換

SVG の `<path>` タグのPath commands文字列(`d` 要素)を変換します

Path commands については [MDN参照](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d#path_commands)

# 使い方

標準入力でパスコマンド文字列を入力します。
今のところ、以下3つの変換に対応しています。

変換 | コマンドライン引数
----|----
座標を相対指定に変換する | `--rel`
座標を絶対指定に変換する | `--abs`
座標をn倍に拡大/縮小する | `--scale {n}`

複数のコマンドライン引数を指定した場合、指定した順序で変換を行います。

#＃ サンプル

座標を相対指定に変換し、さらに2倍に拡大します

```
$ svgdpathconv --rel --scale 2
```

# ビルド

```
$ ./build.sh
```
