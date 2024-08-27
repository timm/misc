for(i=1;i<=NF;++) $i= porter($i)

function porter(word,     step1a, step1b, step1c, step2, step3, step4, step5, stem) {
    stem = word

    # Step 1a
    if (sub(/sses$/, "ss", stem)) ;
    else if (sub(/ies$/, "i", stem)) ;
    else sub(/s$/, "", stem)

    # Step 1b
    if (sub(/eed$/, "ee", stem)) ;
    else if (sub(/(ed|ing)$/, "", stem) && match(stem, /[aeiou]/)) {
        if (sub(/at$/, "ate", stem)) ;
        else if (sub(/bl$/, "ble", stem)) ;
        else if (sub(/iz$/, "ize", stem)) ;
        else if (substr(stem, length(stem)-1, 1) == substr(stem, length(stem), 1))
            stem = substr(stem, 1, length(stem)-1)
        else if (match(stem, /^[^aeiou][aeiou][^aeiou]$/))
            stem = stem "e"
    }

    # Step 1c
    if (sub(/y$/, "i", stem) && match(stem, /[aeiou]/)) ;

    # Step 2
    sub(/ational$/, "ate", stem)
    sub(/tional$/, "tion", stem)
    sub(/enci$/, "ence", stem)
    sub(/anci$/, "ance", stem)
    sub(/izer$/, "ize", stem)
    sub(/abli$/, "able", stem)
    sub(/alli$/, "al", stem)
    sub(/entli$/, "ent", stem)
    sub(/eli$/, "e", stem)
    sub(/ousli$/, "ous", stem)
    sub(/ization$/, "ize", stem)
    sub(/ation$/, "ate", stem)
    sub(/ator$/, "ate", stem)
    sub(/alism$/, "al", stem)
    sub(/iveness$/, "ive", stem)
    sub(/fulness$/, "ful", stem)
    sub(/ousness$/, "ous", stem)
    sub(/aliti$/, "al", stem)
    sub(/iviti$/, "ive", stem)
    sub(/biliti$/, "ble", stem)

    # Step 3
    sub(/icate$/, "ic", stem)
    sub(/ative$/, "", stem)
    sub(/alize$/, "al", stem)
    sub(/iciti$/, "ic", stem)
    sub(/ical$/, "ic", stem)
    sub(/ful$/, "", stem)
    sub(/ness$/, "", stem)

    # Step 4
    sub(/al$/, "", stem)
    sub(/ance$/, "", stem)
    sub(/ence$/, "", stem)
    sub(/er$/, "", stem)
    sub(/ic$/, "", stem)
    sub(/able$/, "", stem)
    sub(/ible$/, "", stem)
    sub(/ant$/, "", stem)
    sub(/ement$/, "", stem)
    sub(/ment$/, "", stem)
    sub(/ent$/, "", stem)
    sub(/ou$/, "", stem)
    sub(/ism$/, "", stem)
    sub(/ate$/, "", stem)
    sub(/iti$/, "", stem)
    sub(/ous$/, "", stem)
    sub(/ive$/, "", stem)
    sub(/ize$/, "", stem)
    # Step 5
    sub(/e$/, "", stem)
    if (length(stem) > 1 && substr(stem, length(stem), 1) == "l" && substr(stem, length(stem)-1, 1) == "l")
        stem = substr(stem, 1, length(stem)-1)
    return stem
}

