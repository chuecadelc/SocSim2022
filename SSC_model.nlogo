extensions[csv nw table rnd palette profiler] ; stats

globals
[
  information
  data-output-interval
  filename
  filename1
  edges   ; record edge list
  attribs ; records turtle attribs
  params
  params1
  data
  onliners
  n-agents
  s ; nat.id change constant
  d ; nat.id similarity threshold
]

undirected-link-breed [offline_friendships offline_friendship]
undirected-link-breed [online_friendships online_friendship]

links-own[weight]
patches-own [pvalue]

turtles-own [

  ;;attributes
  my-information ;; represents the information received by the agent discussing national identity
  nat_id ;; indicates the agent's national identity and consequently group identity
  uncertainty ;; indicates the strength of an agent's convictions about national id
  engagement ;; indicates the engagement levels or socialisation patterns of a given agent

  ;; data collection
  changed? ;; indicates whether the agent has changed their national identity already
  homophily? ;; indicates whether an agent is in a homophilous group or mixed beliefs group
  in-group ;; Proportion of in-group ties, national identity-supportive neighbors per agent
  net_ties ;; Proportion of inter-group ties, national identity-discrepant neighbors per agent
]

to setup

  ca
  set s 0.05 ; fixed for experiments but not for sensitivity
  set d 0.5
  set data-output-interval [0 50 100 150 200 250 300 350 400] ;; when to save model outputs
  setup-information

  ;; Importing different nat.id distributions (normal // CEO 2011 Dataset)
  ifelse Calibrate? = "normdis"
  [
    read-agents-norm
  ]
  [
    read-agents-cali
  ]

  set n-agents count turtles

  ifelse Networks = "random"
  [ ;; importing random net, faster
    import-random-offline
    import-random-online
  ]

  [ ;; rendering the homophilous one since nat.id changes upon setup
    set onliners n-of round (n-agents * 0.8) turtles
    make-initial-links-homophily
    setup-online_turtles-homophily
    setup-links-homophily
  ]

  ;; Data collection purposes

  let Net ifelse-value Networks = "homophilous" [1][0]
  let SM ifelse-value Social-Media = "filter-bubble_ON" [1][0]
  let CEO ifelse-value Calibrate? = "CEO2011" [1][0]

  set params (word behaviorspace-run-number "," Net "," SM  )
  set filename (word word "Net-" Net "_SM-" SM "_CEO-" CEO ".csv")


  ;;creating agent attribs file
  if not file-exists? word "attribs-" filename [
    file-open word "attribs-" filename
    file-print "run,Networks,Social-Media,t,agent,nat_id"
    file-close
  ]

  ;; Saving the initial nat_id distribution
  file-open word "attribs-" filename
  ask turtles [file-print (word params "," 0 "," who "," nat_id)]
  file-close

  ; creating model output file
  if not file-exists? word "output-" filename [
    file-open word "output-" filename
    file-print "run,Networks,Social-Media,t,Nat_ID_variance"
  ]



;;; when running sensitivity analyses -- comment out previous section and uncomment this section

;  set params1 (word behaviorspace-run-number "," Net "," SM "," probs-rewiring "," d "," s )
;  set filename1 (word "Net-" Net "_SM-" SM "_" d "_" s "_CEO-" CEO ".csv")
;
;  if not file-exists? word "attribs-" filename1 [
;    file-open word "attribs-" filename1
;    file-print "run,Networks,Social-Media,d,s,t,agent,nat_id"
;    file-close
;  ]
;
;  ;; Saving the initial nat_id distribution
;  file-open word "attribs-" filename1
;  ask turtles [file-print (word params1 "," 0 "," who "," nat_id)]
;  file-close
;
;  ; creating model output file
;  if not file-exists? word "output-" filename1 [
;    file-open word "output-" filename1
;    file-print "run,Networks,Social-Media,d,s,t,Nat_ID_variance"
;  ]
;

  reset-ticks

end


to go

  ask turtles
  [
    set in-group 0
    set changed? false
    set homophily? false
    set net_ties 0
  ]

  ;; Processes
  information-receival
  homophily_check
  share
  update-natid


  ;; data collection ;;

  if member? ticks data-output-interval  [
    file-open  word "output-" filename ;"output.csv"
    file-print ( word params "," ticks "," Nat_ID_variance)
    file-close
  ]

  if member? ticks data-output-interval  [
    file-open word "attribs-" filename
    ask turtles [file-print (word params "," ticks "," who "," nat_id)]
    file-close
  ]

;; sensitivity analyses data saving procedure -- comment out previous and uncomment this section

;   if member? ticks data-output-interval  [
;    file-open word "attribs-" filename1
;    ask turtles [file-print (word params1 "," ticks "," who "," nat_id )]
;    file-close
;  ]
;
;  if member? ticks data-output-interval  [
;    file-open word "output-" filename1
;    file-print ( word params1 "," ticks "," Nat_ID_variance)
;    file-close
;  ]



  tick

end


;;;;;;;;;;;;;;;;;;
;;;; Networks ;;;;
;;;;;;;;;;;;;;;;;;

to import-random-offline

  foreach csv:from-file "offline_edges.csv" [edge ->
    ask turtle item 0 edge [create-offline_friendship-with turtle item 1 edge [set weight item 2 edge]]
  ]
end

to import-random-online

  foreach csv:from-file "online_edges.csv" [edge ->
    ask turtle item 0 edge [create-online_friendship-with turtle item 1 edge [set weight item 2 edge]]
  ]

end


to save-edgelist

  file-open "online_edges.csv"
  ask online_friendships [file-print (word ticks "," [who] of end1 "," [who] of end2 "," weight)]
  file-close


  file-open "offline_edges.csv"
  ask offline_friendships [file-print (word ticks "," [who] of end1 "," [who] of end2 "," weight)]
  file-close

end



to make-initial-links-homophily

  repeat 60 [
    ask one-of onliners
    [ let nid nat_id
      let online_option onliners with [abs(nat_id - nid) < d]
      ifelse any? online_option
      [
      create-online_friendship-with one-of other online_option  [set color grey set weight random-float 1]
      ]
      [
      create-online_friendship-with one-of other onliners  [set color grey set weight random-float 1]
      ]
    ]
  ]

end


to setup-online_turtles-homophily

  let linked onliners with [count my-online_friendships > 0 ]
  ask onliners [
    let option linked with [abs(nat_id - [nat_id] of myself) < 0.5]
    repeat 23 [
      ifelse random 100 >= 10 [
        ifelse any? option  ;; based on homophily
          [create-online_friendship-with one-of other option [set color white set weight random-float 1]]
        [create-online_friendship-with one-of other linked [set color white set weight random-float 1]]
      ]
      [create-online_friendship-with one-of other onliners  [set color white set weight random-float 1]]
      set linked (turtle-set linked self)
    ]
  ]
  let leftovers onliners with [count my-online_friendships < 27] ;; min avg node degree
  while [any? leftovers][
    ask leftovers [
      let howmany 27 - count my-online_friendships
      if howmany > 0 [   ;; we need this because some leftovers might have received extra links in the meantime
        let option linked with [abs(nat_id - [nat_id] of myself) < 0.5]

        ifelse any? option ;; based on homophily
        [create-online_friendships-with up-to-n-of howmany other option [set color white set weight random-float 1]]
        [create-online_friendships-with up-to-n-of howmany other linked [set color white set weight random-float 1]]
      ]
      set leftovers other leftovers
    ]
  ]

end

to setup-links-homophily

  let average-node-degree 11.5

  let num-links (average-node-degree * n-agents)

  while [count offline_friendships < num-links]
  [
    ask one-of turtles
    [
      create-offline_friendship-with one-of other turtles with [abs(nat_id - [nat_id] of myself) < s] [set color white set weight random-float 1]
    ]
  ]

end


to save-edgelist-H1

  file-open "onlineH_edges.csv"
  ask online_friendships [file-print (word ticks "," [who] of end1 "," [who] of end2 "," weight)]
  file-close

  file-open "offlineH_edges.csv"
  ask offline_friendships [file-print (word ticks "," [who] of end1 "," [who] of end2 "," weight)]
  file-close
end


;;;;;;;;;;;;;;;;;;;;
;;; Turtle Setup ;;;
;;;;;;;;;;;;;;;;;;;;

to read-agents-norm ; reading in agent attributes (from random-normal distribution)

  let row 0
  foreach csv:from-file "data2011_normdis.csv" [ag ->
    let i 1
    if row > 0 [
      crt 1 [
        set nat_id item 1 ag
        set uncertainty item 2 ag
        set engagement item 3 ag
        set-characteristics
      ]
    ]
    set row row + 1
  ]

end


to read-agents-cali

  let row 0
  foreach csv:from-file "data2011_calibrated.csv" [ag ->
    let i 1
    if row > 0 [
      crt 1 [
        set nat_id item 1 ag
        set uncertainty item 2 ag
        set engagement item 3 ag
        set-characteristics
      ]
    ]
    set row row + 1
  ]
end
to set-characteristics

  set color black
  set shape "person"
  setxy random xcor random ycor

;;tracking attributes
  set homophily? false
  set changed? false
  set net_ties 0

;;variable attributes
  set my-information 0

end


to-report random-normal-in-bounds [mid dev mmin mmax]
  let result random-normal mid dev
  if result < mmin or result > mmax
    [ report random-normal-in-bounds mid dev mmin mmax ]
  report result
end

to setup-information ;; creating information pool

  set information []

  let temp range 100
  let pos_information n-values 100 [random-float 1]
  let neg_information n-values 100 [-1 + random-float 1]
  set information sentence pos_information neg_information

end

;;;;;;;;;;;;;;;;;;;
;;;; Processes ;;;;
;;;;;;;;;;;;;;;;;;;

to information-receival

  ask turtles
  [
    set my-information one-of information
  ]

  if Social-Media = "filter-bubble_ON" [filtered-information]

end

to filtered-information ;; when social-networks filter bubble ON

  ask turtles with [any? my-online_friendships]
  [
    if abs(my-information - nat_id) > d
    [
      let max_info nat_id + random-float s
      let min_info nat_id - random-float s
      let information1 ifelse-value (random-float 1 < d) [max_info] [min_info]

      if information1 > 1 [ set information1 1 ]
      if information1 < -1 [ set information1 -1]
      set my-information information1
    ]
  ]
end


to homophily_check ;;currently under review

  ask turtles with [any? online_friendship-neighbors] ;; online echo chambers
  [
    set homophily? false
    set in-group 0

    let my_nat_id nat_id
    let needed (count online_friendship-neighbors) * 0.70

    let my_group online_friendship-neighbors with [abs(nat_id - my_nat_id) < d]
    let my_outgroup online_friendship-neighbors with [abs(nat_id - my_nat_id) > d]

    if any? my_group [set in-group count my_group]
      ; calculating the number of out-group members
    set net_ties count my_outgroup / count online_friendship-neighbors

    if in-group > needed [set homophily? true ]
  ]

end


to change-nat_id [delta]

  ifelse nat_id < 0
  [
    let change_nat nat_id - delta
    if change_nat > 1 [ set change_nat 1 ]
    if change_nat < -1 [ set change_nat -1]

    set nat_id  change_nat

  ]

  [
    let change_nat nat_id + delta
    if change_nat > 1 [ set change_nat 1 ]
    if change_nat < -1 [ set change_nat -1]

    set nat_id  change_nat

  ]

end


to share

  ask turtles [

    let prob-sharing ifelse-value abs(my-information - nat_id) > d and homophily? = true [0.2][1]

    if random-float 1 < engagement and random-float 1 < prob-sharing [

      let off_friends offline_friendship-neighbors  with [not changed?]
      let on_friends online_friendship-neighbors  with [not changed?]

      let caller-info my-information
      let caller self

      if any? on_friends [
        ask on_friends ;; one-to-all communication - online ties
          [
            let wt [weight] of link-with caller
            if abs(caller-info - nat_id) - wt < uncertainty
            [
              set changed? true

              let x (abs(caller-info - nat_id) * s) ;; Nat_id update

              ifelse abs(caller-info - nat_id) < d
              [
                change-nat_id x
              ]
              [
                change-nat_id (0 - x)
              ]
            ]
        ]
      ]

      if any? off_friends [
        ask one-of off_friends ;; one-to-one communication - offline ties
        [
          let wt [weight] of link-with caller
          if abs(caller-info - nat_id) - wt < uncertainty
            [
            set changed? true

            let x (abs(caller-info - nat_id) * s) ;; Nat_Id update

            ifelse abs(caller-info - nat_id) < d
            [
              change-nat_id x
            ]
            [
              change-nat_id (0 - x)
            ]
          ]
        ]
      ]
    ]
  ]

end

to update-natid ; for those agents that didn't update thei nat_if after getting info from their social networks

  let options turtles with [not changed?]

  if any? options
  [
    ask options
    [
      let eval abs(my-information - nat_id)
      let x (abs(my-information - nat_id) * s)

      if eval < uncertainty
      [
        set changed? true

        ifelse eval < d
        [
          change-nat_id x
        ]

        [
          change-nat_id (0 - x)
        ]
      ]
    ]
  ]

end

;;;;;;;;;;;;;;;;;;
;;;; REPORTERS;;;;
;;;;;;;;;;;;;;;;;;


to-report Nat_ID_variance

  let vars variance [nat_id] of turtles
  report vars

end
@#$#@#$#@
GRAPHICS-WINDOW
439
10
935
507
-1
-1
14.8
1
10
1
1
1
0
0
0
1
-16
16
-16
16
0
0
1
ticks
30.0

BUTTON
217
164
281
197
Setup
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

BUTTON
316
165
379
198
Go
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

CHOOSER
13
38
143
83
Networks
Networks
"homophilous" "random"
0

CHOOSER
235
40
393
85
Social-Media
Social-Media
"filter-bubble_ON" "filter-bubble_OFF"
0

TEXTBOX
17
14
311
44
Select the initial network setup
12
0.0
1

TEXTBOX
209
13
431
41
Select the information sorting algorithm
12
0.0
1

CHOOSER
10
163
148
208
Calibrate?
Calibrate?
"normdis" "CEO2011"
1

TEXTBOX
18
112
168
142
Type of initial Nat.Id\ndistribution
12
0.0
1

@#$#@#$#@
# SOCIAL IDENTITY MODEL OF PROTEST EMERGENCE (SIMPE)

The purpose of this model is to simulate national identity polarisation in a given country where an independence movement is present, making national identity salient. This model integrates previous models of opinion dynamics, social influence, and collective action (see Deffuant et al., 2002; Hegselmann and Krause 2002; Mäs and Flache, 2010; Epstein, 2003;2013, Edmonds,2021). The general idea of the model is that individuals receive information from the media and from their social connections. Then, they decide whether or not to share it with their networks and evaluate whether or not to update their national identities.  
  

# HOW IT WORKS

At setup, social networks are created. Offline social networks have a small-world topology with an average node degree of 27 whereas the online social networks have a scale-free topology with an average node degree of 45. Both values have been obtained from empirical studies (see Lubbers et al., 2019 and Dunbar et al., 2015). It should be noted that not every agent has online social networks and the proportion of agents that do have such networks is modelled after the Spanish internet use penetration value since this model focuses on the Catalan secessionist movement.

There are two social network scenarios, random and homophilous. *Random* each agent makes a tie with another agent based on random probability. *Homophilous*: the probability that an agent gets chosen depends on national identity similarity or homophil). This means that initially, ties are formed with those within the similarity threshold (see Deffuant et al., 2000;2004;2008) and then with the remainder of the agents.

Since there are online social networks, there are two scenarios that control the information that agents get exposed to through social media platforms. *Filter-Bubble_ON* represents a media environment where agents get predominantly exposed to national identity-consistent information while also getting exposed to national identity-discrepant information in a smaller proportion. *Filter-Bubble_OFF* represents a diverse media environment where agents get equally exposed to national identity-consistent and national identity-discrepant information.

Since the key component of this model is national identity, two alternative distributions for this variable are provided. *normdis* : each agent has a randomly distributed national identity  ranging from -1 to 1. *CEO2011*: each agent's national identity value was taken from the CEO October 2011 survey dataset (reference at end of document) resulting in a left-skewed distribution ranging from -1 to 1. This scenario is considered calibrated compared to the *normdis* one.

Additionally to national identity a, agents have a randomly distributed grievance value from 0 to 1 which indicates their discontent with the situation and promotes protesting. Costs of participating in protest are also drawn from a random distribution and range from 0 to 1. Similarly, agents have (social) engagement levels, ranging from 0 to 1, representing their socialisation patterns and national identity uncertainty, ranging from 0 to 1, representing the strength of their views and room for persuasion to change their national identities.

At each tick, each agent will receive information whether from another agent or from the media's information hub. Three decisions will be made by agents: sharing the information with their social networks, updating their national identities, and protesting which updates their grievances. 




# HOW TO USE IT


The Social-Media switch enables the filtering algorithm that preferentially sends attitude-supportive information to agents. If off, agents have equal probabilities of receiving attitude-supportive and attitude-discrepant messages.

Networks switch enables the creation of homophilous networks at the start of the simulation, following the bounded confidence principle, links will be created with agents that are similar to oneself. Alternatively, links will be created at random regardless of the national identity similarity between agents.

Press SETUP to populate the world with agents creating offline and online links. GO will run the simulation continuously.

The View panel currently doesn't show the agents in different protest states as well as their ties, grey or white corresponding to offline and online connections because of the large size of the population (N= 2,500) which takes a very long time to render. 

Data-save? allows to save agent attributes and model outputs every 100 steps of the simulation. There are two separate data collection procedures in the Code, one for the general behaviour space and another for sensitivity analyses. You will need to (un)comment accordingly. In any case, it creates a separate file for the model outputs and agent attributes for the parameter combination. This is to allow to read them into R/Python more easily than a single 25GB file.


Press SETUP to populate the world with agents creating offline and online links. GO will run the simulation continuously.


# THINGS TO NOTICE

After running the model for a while, national identities converge towards the centre of the distribution or they divide into two or more distinct clusters. This depends on the combination of initial social network configurations and social media filtering algorithms.


# THINGS TO TRY

How does the presence of filter bubbles affect national identities and protest mobilisation?

How does starting from a random network compared to starting from a homophilous network setup affect national identities?

# CREDITS AND REFERENCES

Acknowledgements to Dr. Stefano Picascia for his technical support and guidance.

Adam-Troian, J, Mahfud Y, Urbanska K, and Guimond S. (2020) The role of social identity in the explanation of collective action: An intergroup perspective on the Yellow Vests movement. Journal of Applied Social Psychology, 21(0), pp.1–17.

Guillaume D., Amblard, F., Weisbuch, A., and Faure, T. (2002) How can extremism prevail? A study based on the relative agreement interaction model. Journal of Artificial Societies and Social Simulation 5(4)1. <http://jasss.soc.surrey.ac.uk/5/4/1.html>

Dunbar, R.I.M., Arnaboldi, V., Conti, M., and Passarella, A. (2015) The structure of online social networks mirrors those in the offline world, Social Networks, 43, pp.39-47, https://doi.org/10.1016/j.socnet.2015.04.005

Edmonds, B. (2021) “A data-informed bounded-confidence opinion dynamics model” (Version 0.3.0). CoMSES Computational Model Library. Retrieved from: https://www.comses.net/codebases/537b40cf-e300-4245-bcc6-427b53515bf6/releases/0.3.0/

Epstein, J. (2002) Modeling civil violence: An agent-based computational approach.
Proceedings of the National Academy of Sciences, 99, pp.7243-7250.
 
Epstein, J. (2013) Agent zero: Toward neurocognitive foundations for generative
social science. Princeton University Press.

Flache, F. (2018) Between monoculture and cultural polarization: Agent-based models of the interplay of social influence and cultural diversity.Journal of Archaeological Methods Theory, 25, pp.996–1023

Flache, A., Mas, M., Feliciani, T., Chattoe-Brown, E., Deffuant, G., Huet, S. and Lorenz, J. (2017) Models of social influence: Towards  thenext frontiers. Journal of Artificial Societies and Social Simulations, 20, pp.1–31

Flaxman, S., and Goel, S., and Rao, J. M (2016) Filter bubbles, echo chambers, and online news consumption, Public Opinion Quarterly, 80(S1), pp.298-320

Hegselmann, R., and Krause, U. (2002). Opinion dynamics and bounded confidence models, analysis, and simulation. Journal of Artificial Societies and Social Simulations, 5, pp. 1-33.

Lubbers, M. (2019) When networks speak volumes: Variation in the size of broader acquaintanceship networks, Social Networks, 56, pp.55-69

Mas and Flache, A. (2013) Differentiation without distancing, explainingbi-polarization  of opinions without negative influence, PLoS ONE, 8:pp.e74516

Noelle-Neumann, E. (1974) The Spiral of Silence a Theory of Public Opinion, Journal of communication, 24(2), pp. 43-51

Sazarulo, L. (2006) A Continuous Opinion Dynamics Model Based on the Principle of Meta-Contrast, Journal of Artificial Societies and Social Simulation, 9(1), pp. 1-29

Tajfel, H. (1984) Intergroup relations, social myths and social justice in social
psychology (H. Tajfel, Ed.). In H. Tajfel (Ed.), The social dimension.
Cambridge: Cambridge University Press.

Turner, J.C. (1991) Social Influence. Bristol: Open University Press.

# COPYRIGHT AND LICENSE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

Chueca Del Cerro, C. (2021) Social Identity Model of Protest Emergence (SIMPE), WEB, University of Glasgow, UK. 

Please cite the NetLogo software as:
Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
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
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Params1" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="401"/>
    <enumeratedValueSet variable="Social-Media">
      <value value="&quot;filter-bubble_ON&quot;"/>
      <value value="&quot;filter-bubble_OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Networks">
      <value value="&quot;homophilous&quot;"/>
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Calibrate?">
      <value value="&quot;normdis&quot;"/>
      <value value="&quot;CEO2011&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Sensi1" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="401"/>
    <enumeratedValueSet variable="Social-Media">
      <value value="&quot;filter-bubble_ON&quot;"/>
      <value value="&quot;filter-bubble_OFF&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Networks">
      <value value="&quot;homophilous&quot;"/>
      <value value="&quot;random&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="d" first="0.1" step="0.1" last="0.8"/>
    <enumeratedValueSet variable="Calibrate?">
      <value value="&quot;normdis&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
0
@#$#@#$#@
