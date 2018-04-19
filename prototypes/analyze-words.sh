#!/usr/bin/env bash

# TODO This has to be run manually first (in the right directory)
# TODO ensure rg is available when automating this
# rg -t cs '\w+' -woN --color never | sort | uniq -c > raw_words_with_counts

# i.e. the words without the count
frequent_words_only() {
  awk '{ if ($1 > 2) print $2 }' raw_words_with_counts
}



filter_too_short() {
  grep -v '^.\{0,2\}$'
}

filter_non_alpha() {
  grep '^[A-Za-z]\+$'
}

filter_noise() {
  sed 's/_/ /g' | trim | filter_non_alpha | filter_too_short
}



# ThisIsAnExampleID -> This Is An Example ID
split_by_case_change() {
  sed -e 's|\([A-Z][^A-Z]\)| \1|g' -e 's|\([a-z]\)\([A-Z]\)|\1 \2|g'
}

trim() {
  sed -e 's/^\s\+//' -e 's/\s\+$//'
}

replace_space_by_newline() {
  sed 's/\s\+/\n/g'
}

to_lower_case() {
  awk '{print tolower($0)}'
}


statistics() {
  wc -l "$1"
  echo "Occurences / Length of words"
  awk '{ print length($0) }' "$1" | sort | uniq -c | sort -n -k2
}



frequent_words_only > raw_words

cat raw_words | filter_noise | trim > filtered_words
cat filtered_words | replace_space_by_newline | filter_noise | split_by_case_change | to_lower_case | sort -u > word_groups
cat word_groups | replace_space_by_newline | filter_noise | sort -u > words

statistics words


# TODO Filter out stuff that didn't appear often

