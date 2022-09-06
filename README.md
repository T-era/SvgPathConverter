# SVG Pathコマンド変換

SVG の `<path>` タグのPath commands文字列(`d` 属性)を変換します

Path commands については [MDN参照](https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/d#path_commands)

# 使い方

標準入力でパスコマンド文字列を入力します。
pathタグの中から、 `d` 属性だけを抜き出して標準入力に指定してください。

今のところ、以下3つの変換に対応しています。

変換 | コマンドライン引数
----|----
座標を相対指定に変換する | `--rel`
座標を絶対指定に変換する | `--abs`
座標をn倍に拡大/縮小する | `--scale {n}`
座標を横n倍、縦m倍に拡大/縮小する | `--scale-xy {n} {m}`
座標の数値を丸めて整数値にする | `--round`
補助点やフラグを除いて、開始点だけを繋いだ直線に変換する | `--skeleton`

複数のコマンドライン引数を指定した場合、指定した順序で変換を行います。

## サンプル

座標を相対指定に変換し、さらに2倍に拡大します。

```bash
$ ./svgpathconv --rel --scale 2 << EOS
M10 10 L10 90 90 90 90 10 10 10
EOS
```

# ビルド

```bash
$ ./build.sh
```
