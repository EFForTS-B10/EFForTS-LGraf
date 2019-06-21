;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
__includes["scr_LGraf/initialization.nls" "scr_LGraf/paint_functions.nls" "scr_LGraf/lut_submodel.nls" "scr_LGraf/output.nls" "scr_LGraf/forest_clusters.nls" "scr_LGraf/inaccessible_submodel.nls" "scr_LGraf/road_submodel.nls" "scr_LGraf/household_submodel.nls" "scr_LGraf/field_submodel.nls" "scr_LGraf/plot_functions.nls"]
extensions[gis profiler]
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
globals
[
  ;; Road related globals
  number-of-roads           ; number of roads
  current_road_radius       ; minimum distance between roads
  warning-road              ; dummy variable to write road warnings to report
  cummulative-road-length   ; so far accumulated road length [cells]
  household-road-ratio      ; number of households/number of road cells
  p_r0                      ; Store points for perlin road creation algorithm
  p_r1                      ; Store points for perlin road creation algorithm
  road-shapefile            ; shapefile of roads
  map-frame-shapefile       ; this shapefile contains only the borders of the road shapefile. Its needed as otherwise areas without roads would be cut out when uploading the road shapefile

  ;; Household globals
  realized-household-area-mean           ; realized mean hh-area in the final generated landscape
  realized-household-area-median         ; realized median hh-area in the final generated landscape
  expected-total-household-area          ; in [cells]
  realized-total-household-area          ; in [cells]
  max-household-size                     ; [cells] maximum household size from the expected-freq-of-household-sizes
  household-sizes                        ; random draw of household sizes [in cells] taken during the setup; the simulation tries to approximate this random draw. emptied when households are created
  household-sizes-list                   ; same as household-sizes, but does not get emptied
  vlg-areas                              ; random draw of village areas during the setup ; JANA
  real-vlg-areas                         ; realised vlg-areas when households are added
  vlg-sizes                              ; list of number of households in village ; JANA
  actual-freq-of-household-sizes         ; frequency distribution of actual household sizes
  actual-freq-of-growing-household-sizes ; freqency of households of a given size wich are still establishing fields (used in determine-still-growing-households)
  still-needed-freq-of-household-sizes   ; frequency of households of given sizes, which are still needed to approximate the expected-freq-of-household-sizes (used in determine-still-growing-households)
  most-common-hh-size                    ; only calculated for output on GUI
  most-common-field-size                 ; only calculated for output on GUI
  local                                  ; number of local households in village ; JANA
  transmigrant                           ; number of transmigrant households in village ; JANA
  real-proportion                        ; proportion of transmigrants in village ; JANA

  ;; Utility globals
  stopper                   ; used to control infinite loops
  current-patch-id          ; each agricultural field has a patch-id; current patch-id is used as a counter that is increased by one for each new agricultural field
  conversion-ha-cell        ; conversion factor from ha to cell (4 in our case)
  cell-size                 ; size of a cell in ha
  total-number-of-cells     ; total number of cells in the world, neede for calculating land-use fractions
  envelope                  ; gis-envelope of the map-frame-shapefile
  x-extent                  ; map extension [m] in x-direction
  y-extent                  ; map extension [m] in y-direction
  number-of-cells-x         ; number of cells in the world in x-direction
  number-of-cells-y         ; number of cells in the world in y-direction
  number-unsuccessful-loops ; number of unsuccessful loops over households which are still supposed to be growing in size. Needs to be global variable to make simulation button stoppable
  strategies-pattern        ; store the numbers of strategies which are used (defined on the gui
  log-list-setup            ; list with warning messages during setup
  log-list-households       ; list with warning messages during household and village creation
  log-list-fields           ; list with warning messages during field establishment
  no-hh-setup               ; number of households that should have been created
  no-vlg-setup              ; number of villages that should have been created
  no-vlg-real               ; number of villages that were actually created
  area-setup                ; proportion of agricultural area that should have been created
  area-real                 ; proportion of agricultural area that was actually created

  ;; LUT related globals
  LUT-names-list                    ; List to store the names which are defined on the interface
  LUT-fractions-list                ; List to store the fractions which are defined on the interface
  LUT-specialize-list               ; List to store the specialize-fractions which are defined on the interface
  LUT-expected-cells                ; List to calculate the amount of expected cells for each LUT
  LUT-realized-cells                ; List to store the realized amount of cells for each LUT
  LUT-realized-fractions            ; List to store the realized fractions for each LUT
  LUT-realized-fractions-specialist ; List to store the realized fractions of specialists for each LUT
  land-use-list-specialists
  expected-cells-specialists
  land-use-list
  expected-cells
  realized-forest-cover             ; percent
  expected-forest-cover             ; fraction, i.e. in [0,1]
  ;intended-forest-cover  ;percent ;potentially intended forest cover could also be used as input, but this interferes then with intended household area. if intended forest cover should be used, one could think about creating households until this forest cover is reached

  list-of-plantation-sizes          ; list where the size [cells] of the inaccessible areas get stored
]
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
turtles-own
[
  homebase-xcor       ; x coordinate of turtles homebase
  homebase-ycor       ; y coordinate of turtles homebase
  homebase-road-nr
  household-size           ; predefined area [cells] of the household
  household-area-realized  ; area [cells] of household under
  household-reached-final-size  ; 0: household is still growing and adding new fields to the household. 1: household has reached its final size in terms of area
  household-number-of-patches   ; households number of fields
  household-number-of-land-use-types ; number of land-use types applied in this household
  household-LUT-patches     ; list to store the amount of household patches for each different LUT
  household-patch-id-list            ; list of patch identity numbers that belongs to this household
  household-search-strat              ;search strategy of farmers for starting plots of fields. different search strategies are defined in field_establishment.nls
  visited-patches               ; List to store the patches which have been visited already
]
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
patches-own
[
 p_landuse           ; land use of the patch. 1:forest; 2: settlement; 3: smallholder field ; 1000 large scale plantation
 p_road              ; 0: no road, 1: road. Roads do not have a spatial dimension, i.e. road patches have at the same time land uses (1, 3, or 1000)
 p_road_nr
 p_owner             ; turtle-id of the household that owns this patch
 p_id                ; patch identity number: this number is the same for the whole field, i.e. cells with the same land use and age belonging to the same household
 p_id_backup         ; store patch ids
 p_homebase          ; list; item 0: 0 if not homebase, village_id if homebase; item 1: number of local households on patch; item 2: number of transmigrant households on patch; This is a subset of road patches, als homebases are only established in road patches
 p_homebase_id       ; 0 if patch is not homebase of somebody, turtle_id, if patch is homebase of this turtle (ToDo: could potentially be merged with p_homebase)
 p_landuse-type      ; 1,2,3 markers for the different land-uses
 p_forestcluster_id  ; identity number for forest cluster. Borders for forest clusters are roads, agricultural fields and large scale plantations
 p_inaccessable_id   ; identity number for the big plantations
 p_elevation         ; to store elevation values generated by perlin noise
 p_elevation_octaves ; stores perlin noise octaves
]


;###################################################################################
; SETUP
;###################################################################################

;Model initialization and creation of roads and households
To setup

  ;; Clear model
  ca
  reset-ticks

  ;; If write param.file = TRUE, write the output:
  if (write.param.file?) [write.param.file]

  ;; Initialize world
  initialize

  if setup-model = "number-of-households"
  [
    if number-of-households = 0 [print "WARNING: set-up number-of-households chosen, but no households created. Create at least one houshold." stop]
    initialize-nr-hh
  ]

  if setup-model = "number-of-villages"
  [
    if number-of-villages = 0 [print "WARNING: set-up number-of-villages chosen, but no villages created. Create at least one village." stop]
    initialize-nr-vlg
  ]

  if setup-model = "agricultural-area"
  [
    if proportion-agricultural-area = 0 [print "WARNING: set-up agricultural-area chosen, but proportion of agricultural area is 0." stop]
    initialize-nr-hh ;; not a typo, nr-hh and agri-area have the same order of procedures
  ]

  ;; Load roads:
  run (word "create.road." road.algorithm)
  calculate-total-road-length

  ;; Create inaccessable-areas
  create-inaccessible-area

  ;; Initialize households
  ;create-and-place-households(number-of-households)

  create-and-place-villages

  ;; Write Problem-Report
  if (print-messages?) [write-report]

  ;; Calculate forest cover
  calculate-forest-cover
  calculate-expected-forest-cover

  ;; Print and Plot
  paint.default.map "household.patches"
  plot-landuses



  ;; Reset the tick counter
  reset-ticks

  print "Setup successful"
End

;###################################################################################
; ESTABLISH FIELDS
;###################################################################################

To establish_fields

  ;; stop prrocedure if no households were created
  if count turtles = 0
  [
    set log-list-fields lput "No fields could be created as there are no households. Think about changing the model parameters." log-list-fields
    write-report-fields
    stop
  ]

  ;; Initialize a list using the defined strategies from the GUI
  initialize-search-strategies

  ;; Run the loop to establish fields
  run-field-establishment-loop

  ;; Calculate real hh-area
  set realized-total-household-area sum [household-area-realized] of turtles

  ;; Calculate new household size frequencies
  calc-freq-of-household-sizes
  calculate-actual_freq-of-field-sizes

  ;; Calculate forest-cover and clusters
  calculate-forest-cover
  calculate-forest-clusters

  ;; Store patch ids in backup variable:
  ask patches [set p_id_backup p_id]

  ;; Update plots on the interface
  plot-landuses

  ;; Write performance report
  if (print-messages?) [write-report-fields]

  ;; Paint output
  paint.default.map "field-patches"

  print "Field establishment successful"
end


;###################################################################################
; ASSIGN LAND USES
;###################################################################################

To assign-land-uses
  ;different options of assigning land uses to fields: either only based on a certain landscape level fraction of land uses
  ;or additionally based on a certain level of specialization of households on certain land uses

  ;; Initialize land use parameters
  init-assign-landuse

  ;; Calculate expected specialze-cells:
  calculate-land-use-lists

  ;; Run assign land use proecdure
  assign-land-use-types

  ;; Do calculations
  calculate-realized-land-uses
  calculate-realized-area-of-specialists

  ;; Paint and update world
  paint.default.map "landuse-type"

  ;; Calculate forestcluster values and write the forestcluster Ids into p_id of patches:
  calculate-forest-clusters
  convert-forestclusterid-to-pid

End

;###################################################################################
; WRITE PERFORMANCE REPORT
;###################################################################################

to write-report
  output-print (word "#### Model Performance Report ####")
  output-print ""

  output-print (word "######## SETUP PARAMETERS ########")
  output-print ""
  if setup-model = "number-of-households"
  [
    output-print "Setup-type: Number-of-Households"
    output-print (word "Number of Households chosen: " no-hh-setup)
    output-print (word "Actually created Households: " count turtles)
  ]

    if setup-model = "number-of-villages"
  [
    output-print "Setup-type: Number-of-Villages"
    output-print (word "Number of Villages chosen: " no-vlg-setup)
    output-print (word "Actually created Villages: " max [item 0 p_homebase] of patches)
  ]

  if setup-model = "agricultural-area"
  [
    output-print "Setup-type: Agricultural-Area"
    output-print (word "Proportion of Agricultural Area chosen: " area-setup)
    output-print (word "Intended Proportion of Agricultural Area: " (sum [household-size] of turtles / count patches))
  ]

  ;; write warning if there has been a problem during road setup
  if warning-road = 1
  [
    set log-list-setup lput "Minimum road distance fell below set value." log-list-setup
    set log-list-setup lput (word "Minimum road distance chosen: " min-dist-roads) log-list-setup
    set log-list-setup lput (word "Actual minimum road distance: " current_road_radius) log-list-setup
  ]

  output-print ""
  output-print "####################################"
  output-print ""
  output-print (word "############# SETUP ##############")
  output-print ""
  ifelse empty? log-list-setup
  [
   output-print (word "Setup ran smoothly")
  ]
  [
    let item-counter 0
    while [item-counter < length log-list-setup]
    [
      output-print item item-counter log-list-setup
      set item-counter item-counter + 1
    ]
  ]


  output-print ""
  output-print "####################################"
  output-print ""
  output-print (word "###### HOUSEHOLD & VILLAGES ######")
  output-print ""
  ifelse empty? log-list-households
  [
   output-print (word "Household creation ran smoothly")
  ]
  [
    let item-counter2 0
    while [item-counter2 < length log-list-households]
    [
      output-print item item-counter2 log-list-households
      set item-counter2 item-counter2 + 1
    ]
  ]
end

to write-report-fields
  output-print ""
  output-print "####################################"
  output-print ""
  output-print (word "############# FIELDS ##############")
  output-print ""
  ifelse empty? log-list-fields
  [
   output-print (word "Field establishment ran smoothly")
  ]
  [
    let item-counter 0
    while [item-counter < length log-list-fields]
    [
      output-print item item-counter log-list-fields
      set item-counter item-counter + 1
    ]
  ]

  if setup-model = "agricultural-area"
  [
    output-print ""
    output-print (word "Proportion of Agricultural Area chosen: " area-setup)
    output-print (word "Intended Proportion of Agricultural Area: " (sum [household-size] of turtles / count patches))
    output-print (word "Actually created Proportion of Agricultural Area: " (sum [household-area-realized] of turtles / count patches))
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
820
570
1428
1179
-1
-1
6.0
1
10
1
1
1
0
0
0
1
0
99
0
99
0
0
1
ticks
30.0

INPUTBOX
245
315
360
375
number-of-households
242.0
1
0
Number

INPUTBOX
30
480
125
540
hh-area-mean-ha
1.0
1
0
Number

INPUTBOX
400
145
495
205
total-road-length
500.0
1
0
Number

PLOT
1350
105
1531
230
Field size distribution
Field size [cells]
Frequency
0.0
20.0
0.0
1.0
false
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""
"pen-1" 1.0 1 -7500403 true "" ""

PLOT
1170
105
1350
230
Household area distribution
Household area [ha]
Frequency
0.0
10.0
0.0
10.0
false
false
"" ""
PENS
"default" 1.0 0 -2674135 true "" ""
"pen-1" 2.0 1 -7500403 true "" ""
"distribution" 1.0 0 -16777216 true "" ""

INPUTBOX
125
480
200
540
hh-area-sd-ha
0.5
1
0
Number

CHOOSER
30
435
200
480
hh-area-distribution
hh-area-distribution
"constant" "normal" "log-normal"
2

CHOOSER
50
720
210
765
field-size-distribution
field-size-distribution
"constant" "uniform" "normal" "log-normal"
3

INPUTBOX
50
765
135
825
field-size-mean-ha
0.49
1
0
Number

INPUTBOX
135
765
210
825
field-size-sd-ha
0.77
1
0
Number

SWITCH
25
145
140
178
reproducable?
reproducable?
0
1
-1000

INPUTBOX
540
585
635
645
inacc-area-mean
0.5
1
0
Number

INPUTBOX
635
585
710
645
inacc-area-sd
19.0
1
0
Number

BUTTON
960
130
1140
173
write output
write-output
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
1350
230
1530
355
Forest patches
Patch size [hal]
Frequency
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" ""

CHOOSER
280
145
395
190
road.algorithm
road.algorithm
"artificial.graffe" "artificial.perlin" "real.shapefile"
2

INPUTBOX
540
525
710
585
inaccessible-area-fraction
0.0
1
0
Number

CHOOSER
540
435
710
480
inaccessible-area-location
inaccessible-area-location
"random" "road-connected"
1

CHOOSER
465
1110
610
1155
land-use-types
land-use-types
"landscape-level-fraction" "household-level-specialization" "spatial-clustering (not there yet)"
0

MONITOR
460
1015
570
1060
 LUT-fractions-sum
precision sum (list LUT-1-fraction LUT-2-fraction LUT-3-fraction LUT-4-fraction LUT-5-fraction) 3
17
1
11

MONITOR
1530
105
1705
150
NIL
LUT-realized-fractions
17
1
11

MONITOR
1530
150
1705
195
NIL
LUT-realized-fractions-specialist
17
1
11

CHOOSER
540
480
710
525
inaccessible-area-distribution
inaccessible-area-distribution
"constant" "uniform" "normal"
2

TEXTBOX
400
690
525
720
2.4) Toggle field establishment strategies:
11
125.0
1

SLIDER
635
245
765
278
households-per-cell
households-per-cell
1
10
3.0
1
1
NIL
HORIZONTAL

PLOT
1530
195
1705
320
Landuse Fractions
NIL
NIL
0.0
6.0
0.0
1.0
false
true
"" ""
PENS
"matrix" 1.0 1 -11053225 true "" ""
"settlement" 1.0 1 -6459832 true "" ""
"agriculture" 1.0 1 -13791810 true "" ""
"inaccessible" 1.0 1 -2674135 true "" ""
"cumulative" 1.0 1 -16777216 false "" ""
"expected" 1.0 0 -16777216 true "" ""

CHOOSER
960
175
1140
220
write-household-ids
write-household-ids
"only-first-households" "layered-files"
0

BUTTON
965
510
1140
543
NIL
create-3D-map
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
960
220
1140
280
foldername
output
1
0
String

INPUTBOX
50
1030
130
1090
LUT-1-fraction
0.4
1
0
Number

INPUTBOX
130
1030
210
1090
LUT-2-fraction
0.4
1
0
Number

INPUTBOX
210
1030
290
1090
LUT-3-fraction
0.2
1
0
Number

INPUTBOX
50
1110
130
1170
LUT-1-specialize
0.0
1
0
Number

INPUTBOX
130
1110
210
1170
LUT-2-specialize
0.0
1
0
Number

INPUTBOX
210
1110
290
1170
LUT-3-specialize
0.0
1
0
Number

INPUTBOX
25
175
140
235
rnd-seed
-1.570439526E9
1
0
Number

INPUTBOX
210
950
290
1010
LUT-3-name
junglerubber
1
0
String

INPUTBOX
50
950
130
1010
LUT-1-name
oilpalm
1
0
String

INPUTBOX
130
950
210
1010
LUT-2-name
rubber
1
0
String

INPUTBOX
290
950
370
1010
LUT-4-name
na
1
0
String

INPUTBOX
370
950
450
1010
LUT-5-name
na
1
0
String

INPUTBOX
290
1030
370
1090
LUT-4-fraction
0.0
1
0
Number

INPUTBOX
370
1030
450
1090
LUT-5-fraction
0.0
1
0
Number

INPUTBOX
290
1110
370
1170
LUT-4-specialize
0.0
1
0
Number

INPUTBOX
370
1110
450
1170
LUT-5-specialize
0.0
1
0
Number

TEXTBOX
285
115
395
145
1.3) Choose road creation algorithm
11
105.0
1

TEXTBOX
250
295
660
313
1.7) Define nr. of households OR nr. of villages OR proportion of agricultural area
11
105.0
1

TEXTBOX
35
405
195
431
1.8) Define Household parameters
11
105.0
1

TEXTBOX
545
395
695
425
1.12) Define Inaccessible area parameters (e.g. plantations)
11
105.0
1

TEXTBOX
50
690
200
716
2.1) Define field size distribution
11
125.0
1

TEXTBOX
405
115
500
151
1.3.2) artificial: set road parameters
11
105.0
1

TEXTBOX
55
935
260
953
3.1) Define names for up to 5 land uses
11
25.0
1

TEXTBOX
55
1015
400
1033
3.2) Define fractions for up to 5 land uses (sum has to be 1!)
11
25.0
1

TEXTBOX
55
1095
485
1113
3.3) For each LUT, define fraction of households that cultivate this LUT exclusively
11
25.0
1

CHOOSER
460
970
570
1015
LUT-fill-up
LUT-fill-up
"LUT-1-fraction" "LUT-2-fraction" "LUT-3-fraction" "LUT-4-fraction" "LUT-5-fraction"
0

TEXTBOX
465
925
560
975
3.4) Define LUT type for filling up fractions if sum < 1
11
25.0
1

TEXTBOX
465
1075
610
1105
3.5) Define LUT distribution algorithm
11
25.0
1

TEXTBOX
30
125
135
143
1.1) Set random seed
11
105.0
1

TEXTBOX
150
110
240
145
1.2) Set world extent and grain
11
105.0
1

INPUTBOX
145
145
210
205
width
100.0
1
0
Number

INPUTBOX
210
145
275
205
height
100.0
1
0
Number

INPUTBOX
145
205
275
265
cell-length-meter
50.0
1
0
Number

INPUTBOX
530
825
705
885
change-strategy
2.0
1
0
Number

SLIDER
500
145
630
178
perlin-octaves
perlin-octaves
1
12
3.0
1
1
NIL
HORIZONTAL

SLIDER
500
180
630
213
perlin-persistence
perlin-persistence
0
1
0.1
0.01
1
NIL
HORIZONTAL

INPUTBOX
400
205
495
265
min-dist-roads
5.0
1
0
Number

CHOOSER
795
290
950
335
paint-cells
paint-cells
"landuse" "landuse-type" "id" "owner" "forestcluster_id"
1

SWITCH
795
380
950
413
paint-homebase-cells?
paint-homebase-cells?
1
1
-1000

SWITCH
795
410
950
443
paint-road-cells?
paint-road-cells?
1
1
-1000

CHOOSER
795
335
950
380
label-cells
label-cells
"" "id" "owner" "forestcluster_id"
0

BUTTON
795
245
950
290
NIL
paint.all
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
795
440
950
473
show-households?
show-households?
1
1
-1000

CHOOSER
795
175
950
220
default.maps
default.maps
"forest-non-forest" "landuse" "landuse-type" "field-patches" "household-patches" "forestcluster"
4

BUTTON
795
130
950
175
paint.default.map
paint.default.map default.maps
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
400
720
525
753
s1.homebase
s1.homebase
0
1
-1000

SWITCH
400
755
525
788
s2.fields
s2.fields
0
1
-1000

SWITCH
400
790
525
823
s3.nearby
s3.nearby
0
1
-1000

SWITCH
400
825
525
858
s4.avoid
s4.avoid
0
1
-1000

TEXTBOX
510
115
625
151
1.3.3) artificial.perlin: set parameters
11
105.0
1

SLIDER
500
215
630
248
cone-angle
cone-angle
90
200
90.0
1
1
NIL
HORIZONTAL

SLIDER
500
250
630
283
dist-weight
dist-weight
0
1
0.0
0.01
1
NIL
HORIZONTAL

SLIDER
460
315
640
348
proportion-agricultural-area
proportion-agricultural-area
0
1
0.3
0.01
1
NIL
HORIZONTAL

TEXTBOX
800
110
950
128
Paint default maps
11
0.0
1

TEXTBOX
800
225
955
243
Define own paint settings
11
0.0
1

TEXTBOX
965
115
1115
133
Write output files
11
0.0
1

TEXTBOX
970
495
1055
513
Utility functions
11
0.0
1

PLOT
1170
230
1350
355
Inaccessible area distribution
Patch size [ha]
Frequency
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"pen-0" 1.0 1 -7500403 true "" ""
"default" 0.5 0 -16777216 true "" ""

SLIDER
215
830
390
863
field.shape.factor
field.shape.factor
0.5
5
1.0
0.1
1
NIL
HORIZONTAL

SWITCH
530
720
705
753
set-field-strategies-by-id?
set-field-strategies-by-id?
1
1
-1000

SLIDER
530
755
705
788
field-strategies-id
field-strategies-id
1
8
1.0
1
1
NIL
HORIZONTAL

SWITCH
795
510
940
543
write.param.file?
write.param.file?
1
1
-1000

SWITCH
215
720
390
753
use-field-size-percentage?
use-field-size-percentage?
1
1
-1000

SLIDER
215
755
390
788
field-size-percentage
field-size-percentage
0
1
0.0
0.01
1
NIL
HORIZONTAL

PLOT
1170
355
1350
480
Village size distribution
Village size [n hhs]
Frequency
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 2.0 0 -16777216 true "" ""
"pen-1" 2.0 1 -7500403 true "" ""

TEXTBOX
210
400
340
430
1.9) Define Village Parameters [area in ha]
11
105.0
1

TEXTBOX
220
685
395
726
2.2) Optionally set field-size-mean relative to household-area-mean-ha
11
125.0
1

TEXTBOX
220
795
385
825
2.3) Adjust field.shape.factor to control range of field shapes
11
125.0
1

TEXTBOX
535
685
735
715
2.5) Optionally set a field strategies pattern from a list by using an id number
11
125.0
1

TEXTBOX
800
480
930
510
Create automatic Parameter Output File?
11
0.0
1

SLIDER
785
570
818
716
zoom
zoom
1
8
6.0
1
1
NIL
VERTICAL

TEXTBOX
380
390
520
431
1.11) Optional: Choose a probability that household type 2 occurs in a village
11
105.0
1

INPUTBOX
380
565
460
625
hh-type-mean
0.56
1
0
Number

INPUTBOX
460
565
535
625
hh-type-sd
0.24
1
0
Number

TEXTBOX
380
475
505
516
1.11.2) Define household parameters for household-type-2
11
105.0
1

CHOOSER
380
520
535
565
hh-distribution
hh-distribution
"uniform" "log-normal" "normal"
2

TEXTBOX
210
545
350
586
1.10) Define minimum distance [cells] between villages
11
105.0
1

SLIDER
210
590
370
623
min-distance
min-distance
0
25
10.0
1
1
NIL
HORIZONTAL

CHOOSER
45
335
175
380
setup-model
setup-model
"number-of-households" "number-of-villages" "agricultural-area"
2

TEXTBOX
25
290
235
330
1.6) Choose whether the number of households, the number of villages or the proportion of agricultural area is fixed
11
105.0
1

CHOOSER
205
435
375
480
vlg-area-distribution
vlg-area-distribution
"constant" "uniform" "normal" "lognormal"
2

INPUTBOX
205
480
357
540
vlg-area-mean
10.0
1
0
Number

INPUTBOX
290
480
375
540
vlg-area-sd
3.0
1
0
Number

INPUTBOX
360
315
460
375
number-of-villages
77.0
1
0
Number

TEXTBOX
640
215
760
245
1.5) Define maximum nr. of households per cell
11
105.0
1

PLOT
1350
355
1530
480
Village area distribution
village area [cells]
frequency
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""
"pen-1" 1.0 1 -7500403 true "" ""

INPUTBOX
635
145
750
205
counter-nr
500.0
1
0
Number

TEXTBOX
640
101
755
146
1.4) Set number of trials until a procedure stuck in a loop stops
11
105.0
1

OUTPUT
1705
105
2135
485
11

TEXTBOX
1555
330
1705
348
Performance Report
14
0.0
1

SWITCH
1550
350
1692
383
print-messages?
print-messages?
0
1
-1000

TEXTBOX
285
195
395
230
1.3.1) real.shapefile: select map id
11
105.0
1

SLIDER
380
435
530
468
occ-probability
occ-probability
0
1
0.0
0.01
1
NIL
HORIZONTAL

TEXTBOX
390
10
605
75
╔════════════════════════╗\n║                                                                ║\n║                                                                ║\n║                                                                ║\n╚════════════════════════╝
11
105.0
1

TEXTBOX
530
795
705
825
2.6) Define nr. of unsuccessful tries before strategy is switched
11
125.0
1

TEXTBOX
10
85
780
105
╔══════════════════════════════════════════════════════════════════════════════════════════════╗
11
105.0
1

TEXTBOX
25
280
755
298
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
11
105.0
1

TEXTBOX
25
380
755
398
--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
11
105.0
1

TEXTBOX
10
100
25
646
║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║
11
105.0
1

TEXTBOX
10
645
780
663
╚══════════════════════════════════════════════════════════════════════════════════════════════╝
11
105.0
1

TEXTBOX
770
100
785
646
║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║
11
105.0
1

TEXTBOX
30
80
345
105
I. Setup landscape and households
20
105.0
0

TEXTBOX
10
665
785
683
╔══════════════════════════════════════════════════════════════════════════════════════════════╗
11
125.0
1

TEXTBOX
770
680
785
916
║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║
11
125.0
1

TEXTBOX
10
680
25
886
║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║
11
125.0
1

TEXTBOX
10
885
780
903
╚══════════════════════════════════════════════════════════════════════════════════════════════╝
11
125.0
1

TEXTBOX
30
660
190
680
II. Establish fields
20
125.0
0

TEXTBOX
10
905
780
923
╔══════════════════════════════════════════════════════════════════════════════════════════════╗
11
25.0
1

TEXTBOX
10
920
25
1171
║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║
11
25.0
1

TEXTBOX
10
1170
780
1188
╚══════════════════════════════════════════════════════════════════════════════════════════════╝
11
25.0
1

TEXTBOX
770
915
785
1166
║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║
11
25.0
1

TEXTBOX
30
900
360
925
III. Distribute crops (land-use types)
20
25.0
0

BUTTON
405
25
585
65
I. Setup landscape and households
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
595
10
805
75
╔════════════════════════╗\n║                                                                ║\n║                                                                ║\n║                                                                ║\n╚════════════════════════╝
11
125.0
1

BUTTON
610
25
790
65
II. Establish fields
establish_fields
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
800
10
1010
75
╔════════════════════════╗\n║                                                                ║\n║                                                                ║\n║                                                                ║\n╚════════════════════════╝
11
25.0
1

BUTTON
815
25
995
65
III. Distribute crops
assign-land-uses
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
230
35
385
55
»» Run model »»
20
0.0
1

TEXTBOX
10
10
220
80
╔════════════════════════╗\n║                                                                ║\n║                                                                ║\n║                                                                ║\n╚════════════════════════╝
11
54.0
1

TEXTBOX
25
30
210
60
EFForTS-LGraf
29
54.0
1

TEXTBOX
780
100
795
551
║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║
11
0.0
1

TEXTBOX
780
85
1165
111
╔═════════════════════════════════════════════╗
11
0.0
1

TEXTBOX
780
550
1160
568
╚═════════════════════════════════════════════╝
11
0.0
1

TEXTBOX
1150
100
1165
561
║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║
11
0.0
1

TEXTBOX
795
80
965
105
IV. Paint & Output
20
0.0
0

TEXTBOX
1160
85
2160
103
╔══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╗
11
0.0
1

TEXTBOX
1160
490
2160
508
╚══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════╝
11
0.0
1

TEXTBOX
1160
100
1175
491
║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║
11
0.0
1

TEXTBOX
2145
100
2160
491
║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║\n║
11
0.0
1

TEXTBOX
1175
75
1325
96
V. Model Output
20
0.0
0

INPUTBOX
280
225
395
285
road-map-id
jambi3
1
0
String

SWITCH
960
285
1140
318
apply-gis-envelope?
apply-gis-envelope?
0
1
-1000

INPUTBOX
960
315
1140
375
gis-envelope
[238244.58 243244.58 9789775.28 9794775.28]
1
0
String

SWITCH
960
380
1140
413
apply-projection?
apply-projection?
0
1
-1000

INPUTBOX
960
410
1140
470
projection-file
input/LGraf_roadmaps/jambi1_road.prj
1
0
String

@#$#@#$#@
## WHAT IS IT?

The general goal of the EFForTS-LGraf landscape generator is to create artificial maps of landscapes that are dominated, or strongly shaped, by agricultural activities. The grid-based maps include fields of various sizes and different crop types and other potential land-cover types as desired. These other potential land-cover types, such as forest, grassland, water bodies or degraded land are grouped into a single land-cover type (here, we used the general term 'others'). In addition to fields, the model considers land ownership by assigning each field to a farming household agent. Artificial land-cover maps produced by EFForTS-LGraf can be used as a template, or input, for other models which, for example, can simulate the effects of land-use types on ecological and/or economic functions. The resulting maps may also be used as a starting point to analyse how farmer decisions alter land-use and shape land-use changes.

## HOW IT WORKS

For details on model functionality, please check out the related publication:


EFForTS-LGraf: A Landscape Generator for Creating Smallholder-Driven Land-Use Mosaics
Jan Salecker1,*, Claudia Dislich1¤, Kerstin Wiegand1,2, Katrin M. Meyer1, Guy Pe´er3,4,5

1 Ecosystem Modelling, Faculty of Forest Sciences and Forest Ecology, University of Goettingen, Germany
2 Centre of Biodiversity and Sustainable Land Use (CBL), University of Goettingen,
Germany
3 Synthesis Centre (sDiv) of the German Centre for Integrative Biodiversity Research
(iDiv) Halle-Jena-Leipzig, Leipzig, Germany
4 UFZ - Helmholtz Centre for Environmental Research, Dept. Economics and Dept.
Ecosystem Services, Leipzig, Germany
5 University of Leipzig, Germany
¤Current Address: Graduate School HIGRADE, Helmholtz Centre for Environmental
Research - UFZ, Leipzig, Germany
* corresponding author: jsaleck@gwdg.de


## CREDITS AND REFERENCES

Copyright 2019. Jan Salecker, Claudia Dislich, Kerstin Wiegand, Katrin M. Meyer, Guy Pe´er. All rights reserved.

This work is licensed under a Creative Commons Attribution-NonCommercial-NoDerivatives 4.0 International License.

http://creativecommons.org/licenses/by-nc-nd/4.0/

Contact jsaleck(at)gwdg.de for license related questions.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

line2
false
0
Line -7500403 true 150 0 150 300

lut_forest
false
5
Circle -10899396 true true 118 3 94
Rectangle -6459832 true false 135 195 165 300
Circle -10899396 true true 80 6 108
Circle -10899396 true true 133 58 92
Circle -10899396 true true 75 75 90
Circle -10899396 true true 105 120 90

lut_op
false
5
Rectangle -6459832 true false 135 210 165 315
Polygon -10899396 true true 150 210 120 180 75 150 45 165 15 210 30 165 75 135 120 150
Polygon -10899396 true true 150 210 180 180 225 150 255 165 285 210 270 165 225 135 180 150
Polygon -10899396 true true 150 210 120 150 75 120 60 120 0 165 45 120 90 105 135 135
Polygon -10899396 true true 150 210 180 150 225 120 240 120 270 165 255 120 210 105 165 135
Polygon -10899396 true true 150 210 165 120 225 90 270 105 300 135 270 90 225 75 150 120
Polygon -10899396 true true 150 210 135 120 75 90 30 105 0 135 30 90 75 75 150 120
Polygon -6459832 true false 150 195 135 210 165 210

lut_ru
false
5
Polygon -6459832 true false 150 195 120 150 90 120 75 60 90 105 150 180 150 195
Polygon -6459832 true false 150 195 180 150 210 120 225 60 210 105 150 180 150 195
Rectangle -6459832 true false 135 180 165 315
Polygon -6459832 true false 150 165 135 180 165 180
Polygon -6459832 true false 150 180 180 135 210 105 225 45 210 90 150 165 150 180
Polygon -6459832 true false 150 180 120 135 90 105 75 60 90 90 150 165 150 180
Polygon -6459832 true false 150 180 165 135 165 105 180 30 165 75 150 165 150 180
Polygon -6459832 true false 150 180 135 135 135 105 120 30 135 75 150 165 150 180
Circle -10899396 true true 99 54 42
Circle -10899396 true true 86 11 67
Circle -10899396 true true 84 84 42
Circle -10899396 true true 129 24 42
Circle -10899396 true true 114 114 42
Circle -10899396 true true 144 54 42
Circle -10899396 true true 161 41 67
Circle -10899396 true true 165 30 30
Circle -10899396 true true 129 9 42
Circle -10899396 true true 75 60 30
Circle -10899396 true true 120 75 30
Circle -10899396 true true 180 105 30
Circle -10899396 true true 105 120 30
Circle -10899396 true true 144 99 42
Circle -10899396 true true 120 60 30

lut_settlement
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -13791810 true false 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.3
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
