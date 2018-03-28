#!/usr/bin/env bash
find '/media/jesse/Files/Dropbox/Prospectus/TextProcessing/RawArticleData' -mindepth 2 -type f -exec rename s/'DOC'/'doc'/g * {} \;
find '/media/jesse/Files/Dropbox/Prospectus/TextProcessing/RawArticleData' -mindepth 2 -type f -exec unoconv -f txt *.doc {} \;

logdir=/media/jesse/Files/Dropbox/Prospectus/TextProcessing/RawArticleData
for dir in "$logdir"/*/
do
	cd "$dir"
	filename="${PWD##*/}"
	newdir="${PWD}"
  for dir2 in "$newdir"/*/
  do
      cd "$dir2"
      files=( *.txt )
      cat "${files[@]}" > "/media/jesse/Files/Dropbox/Prospectus/TextProcessing/FormattedArticleData/$filename${PWD##*/}.txt"
  done
done
