#!/bin/sh

cat owl-first-regular-season-results.json | jq 'foreach .stage[] as $stage ([]; [{stage: [
    $stage.weeks[]
    | {matches:
        (.matches
            | map({team1: .teams[0].name,
                   t1score: .teams[0].score,
                   team2: .teams[1].name,
                   t2score: .teams[1].score
                  }))}]}]; .[])' > results.json
