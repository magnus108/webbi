# webbi

- lav css
- flyt html til menu
- clean routes
- fix cv
- lav post




# Struktur
index                 - skal kunne indeholde billeder
cv/index
post/index
post/applicative/index
post/tests/index
projects/chair/index  - skal have preview
projects/table/index  - skal have preview
projects/table/legs/index- skal have preview
projects/table/top/index- skal have preview
projects/kitchen/door/handle/index- skal have preview


#menu
-index(home)-           -- specielt navn og position
cv / posts / projects  -- ingen valgt


"index(home)"           -- specielt navn og position
-cv- / posts / projects -- valgt
- CV

"index(home)"           -- specielt navn og position
cv / "posts" / projects -- valgt
applicative / -tests-                 -- valgt
- test

"index(home)"           -- specielt navn og position
cv / posts / -projects- -- valgt
chair - preview
table - preview
kitchen / door / handle - preview

"index(home)"           -- specielt navn og position
cv / posts / "projects" -- valgt
chair / -table- / kitchen
legs / top
- table


"index(home)"           -- specielt navn og position
cv / posts / "projects" -- valgt
chair / table / -kitchen-
-door-
-handle-
- handle

# Zipper

           "index"
    /         |            \
cv          posts          projects



            "index"
    /         |            \
"cv"         posts          projects




           "index"
    /         |            \
cv         "posts"          projects

       /             \
    applicative     "tests"


           "index"
    /         |            \
cv         posts            "projects"
                         /     |        \
                    chair    table     kitchen


           "index"
    /         |            \
cv         posts            "projects"
                         /     |        \
                    chair    table     "kitchen"
                                            |
                                        "door"
                                            |
                                        "handle"
