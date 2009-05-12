#! ruby -Ks

# make_oyaji_list.rb
# 字通の本文データと親字検索見出しリストから
# 読み付き見出しリストを生成する

# simplify_font: 漢字表記のフォント指定を簡略化する
def simplify_font(kanji)
    kanji.gsub!(/<\/ST>/, "》")
    kanji.gsub!(/<ST,(12|13|14|51)>/, "《<\\1>")
    kanji.gsub!(/<ST,2([45])>/, "《<2\\1>")
    kanji.gsub!(/<ST,(11|3[345]|41)>/, "《<11>")
    kanji.gsub!(/<ST,[0-9]+>/, "《")
    kanji.gsub!(/《(［|］|（|）)》/,'\1')
end
#def simplify_font(kanji)
#    kanji.gsub!(/<\/ST>/, "")
#    kanji.gsub!(/<ST,(12|13|14|51)>/, "<\\1>")
#    kanji.gsub!(/<ST,2([45])>/, "<2\\1>")
#    kanji.gsub!(/<ST,(11|3[345]|41)>/, "<11>")
#    kanji.gsub!(/<ST,[0-9]+>/, "")
#end

# decrypt: dat/lstエントリの内容を平文にする
def decrypt(ent, len)
    # 先頭から4の倍数バイトのあいだ、4バイトごとに
    # 4バイトBEの値とみなして0xffffffffをXORし、
    # さらに0x8831b311を符号なし加算する
    len4 = len & ~3
    arr = ent[0, len4].unpack("N*")
    arr.length.times { |i|
	arr[i] = ((arr[i] ^ 0xffffffff) + 0x8831b311) & 0xffffffff
    }
    ent[0, len4] = arr.pack("N*")

    # 余りのバイトは0xffとXORする
    i = len4
    while i < len
	ent[i] ^= 0xff
	i += 1
    end

    # 0x00や0xffの直前までを結果として返す
    i = 0
    while i < len && ent[i] != 0x00 && ent[i] != 0xff
	i += 1
    end
    return ent[0, i]
end

# get_yomi: datエントリの2行目(読み)を取り出す
def get_yomi(ent)
    n1 = ent.index(0x0d)
    n2 = ent.index(0x0d, n1 + 1)
    return ent[n1 + 1 ... n2]
end

# add_yomi_tag: <yomi>タグをエントリに付加する。
def add_yomi_tag(yomi)
  yomi.gsub!(/］|・/,'\&<yomi>')
  yomi.gsub!(/(　?［字訓］)|・|（|\z/,'</yomi>\&')
end

# メイン

# dat/lstファイル名
HONMON_DAT = "dat/honmon.dat"
OYAJI_LST = "lst/oyaji.lst"

# dat/lstファイルをオープンする
if ARGV[0] == nil then
    STDERR.print "Usage: make_oyaji_list.rb data_directory\n"
    exit(1)
end
dat_path = ARGV[0] + "/" + HONMON_DAT
lst_path = ARGV[0] + "/" + OYAJI_LST
begin
    datf = File.open(dat_path, "rb")
    lstf = File.open(lst_path, "rb")
rescue
    STDERR.print "Can't open dat/lst files\n"
    exit 1
end

# dat/lstエントリ数を取得する
ent_num = datf.read(4).unpack("V1")[0]

# datエントリ表を読み込む
dat_pos = Array.new(ent_num)
dat_len = Array.new(ent_num)
ent_num.times { |i|
    dat_pos[i] = datf.read(4).unpack("V1")[0]
    dat_len[i] = datf.read(4).unpack("V1")[0]
}

# lstエントリサイズ/文字列サイズを取得する
lstf.seek(16, IO::SEEK_SET)
lst_ent_len = lstf.read(4).unpack("V1")[0]
lst_str_len = lstf.read(4).unpack("V1")[0]
lstf.seek(32, IO::SEEK_SET)

# dat/lstエントリを全件読み、
# エントリ番号とともに整形して出力する
ent_num.times { |i|
    # lstエントリを読んで平文にする
    list = lstf.read(lst_str_len)
    list = decrypt(list, lst_str_len)
    dummy = lstf.read(lst_ent_len - lst_str_len)

    # 漢字表記のフォント指定を簡略化する
    simplify_font(list)

    # datエントリを読んで平文にし、2行目だけを取り出す
    # 本文で必要なのは2行目だけなので先頭160バイトだけ読む
    datf.seek(dat_pos[i], IO::SEEK_SET)
    data = datf.read(160)
    data = decrypt(data, 160)
    data = get_yomi(data)
    data = add_yomi_tag(data)

    # lstエントリとdatエントリを出力する
    printf "%04d %s %s\n", i+1, list, data
}

# 後始末
datf.close
lstf.close
