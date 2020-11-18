;;; -*- Mode:LISP; Package:HACKS; Base:8 -*-
;;; Doctor script from MIT-MULTICS:>nbdd>bsg>m>doctor::doctor.script
;;; BSG hacks for readtables removed 12/18/80 by DLW.
;;; Slashified comma after the word "beaks".

(comment doctor script)

(defprop sorry 0 priority)

(defprop sorry
         (((0) (nil)
               (please don/'t apologize)
               (apologies are not necessary)
               (what feelings do you have when you apologize)
               (I/'ve told you that apologies are not required)
               (apologies are not necessary/, please go on)))
         rules)

(defprop dont don/'t translation)

(defprop cant can/'t translation)

(defprop wont won/'t translation)

(defprop remember 5 priority)

(defprop remember
         (((0 you remember 0) (nil)
                              (do you often think of 4)
                              (does thinking of 4 bring anything else to mind)
                              (what else do you remember)
                              (why do you remember 4 just now)
                              (what in the present situation reminds you of 4)
                              (what is the connection between me and 4))
          ((0 do I remember 0) (nil)
                               (did you think I would forget 5)
                               (why do you think I should recall 5 now)
                               (what about 5)
                               what
                               (you mentioned 5))
          ((0) (nil) newkey))
         rules)

(defprop if 3 priority)

(defprop if
         (((0 if 0 had 0) (nil) (pre (1 2 3 might have 5) if))
          ((0 if 0) (nil)
                    (do you think its likely that 3)
                    (do you wish that 3)
                    (what do you think about 3)
                    (really if 3)))
         rules)

(defprop dreamt 4 priority)

(defprop dreamt
         (((0 you dreamt 0) (nil)
                            (really 4)
                            (have you ever fantasied 4 while you were awake)
                            (have you dreamt 4 before)
                            dream
                            newkey)
         ((0) (nil) dream newkey))
         rules)

(defprop dreamed dreamt translation)

(defprop dreamed 4 priority)

(defprop dreamed (dreamt) rules)

(defprop dream 3 priority)

(defprop dream
         (((0)
           (nil)
           (what does that dream suggest to you)
           (do you dream often)
           (what persons appear in your dreams)
           (do/'t you believe that dream has something to do with your problem)
           (do you ever wish you could flee from reality)
           newkey))
         rules)

(defprop dreams dream translation)

(defprop dreams 3 priority)

(defprop dreams (dream) rules)

(defprop what 0 priority)

(defprop what
        (((what where)
          (nil)
          how)
         ((0 (what where) 0)
          (nil)
          (tell me about 2 3)
          (2 3)
          (do you want me to tell you 2 3)
          (really)
          (I see)
          newkey))
        rules)

(defprop alike 12 priority)

(defprop alike (dit) rules)

(defprop same 12 priority)

(defprop same (dit) rules)

(defprop certainly 0 priority)

(defprop certainly (yes) rules)

(defprop feel t belief)

(defprop think t belief)

(defprop believe t belief)

(defprop wish t belief)

(defprop bet t belief)

(defprop my
         (((0 your 1 0)
           (nil)
           (earlier you said your 3 4)
           (but your 3 4)
           (does that have anything to do with your statement about 3 4)))
         memr)


(defprop perhaps 0 priority)

(defprop perhaps
         (((0) (nil)
               (you don/'t seem quite certain)
               (why the uncertain tone)
               (can/'t you be more positive)
               (you aren/'t sure)
               (don/'t you know)))
         rules)

(defprop maybe 0 priority)

(defprop maybe (perhaps) rules)

(defprop name 17 priority)

(defprop name
         (((0)
           (nil)
           (I am not interested in names)
           (I/'ve told you before I don/'t care about names /- please continue)))
         rules)

(defprop deutsch 0 priority)

(defprop deutsch (((0) (nil) (I am sorry/, I speak only english))) rules)

(defprop francais 0 priority)

(defprop francais (deutsch) rules)

(defprop svenska 0 priority)

(defprop svenska (deutsch) rules)

(defprop Italiano 0 priority)

(defprop Italiano (deutsch) rules)

(defprop espanol 0 priority)

(defprop espanol (deutsch) rules)

(defprop hello 0 priority)

(defprop hello (((0) (nil) (how do you do/. please state your problem))) rules)

(defprop computer 62 priority)

(defprop computer
         (((0) (nil)
               (do computers worry you)
               (why do you mention computers)
               (what do you think machines have to do with your problem)
               (don/'t you think computers can help people)
               (what about machines worries you)
               (what do you think about machines)))
         rules)

(defprop machine 62 priority)

(defprop machine (computer) rules)

(defprop machines 62 priority)

(defprop machines (computer) rules)

(defprop computers 62 priority)

(defprop computers (computer) rules)

(defprop am 0 priority)

(defprop am are translation)

(defprop am
         (((0 are you 0) (nil)
                         (do you believe you are 4)
                         (would you want to be 4)
                         (you wish I would tell you you are 4)
                         (what would it mean if you were 4)
                         how)
          ((0) (nil) (why do you say /'am/') (I don/'t understand that)))
         rules)

(defprop are 0 priority)

(defprop are
         (((0 there are 0 you 0) (nil) (pre (1 2 3 4) are))
          ((0 there are 1 0) (nil)
                             (what makes you think 2 3 4 5)
                             (do you usually consider 4 5)
                             (do you wish 2 were 4 5))
          ((0 there are 0) (nil) newkey)
          ((0 are I 0) (nil)
                       (why are you interested in whether I am 4 or not)
                       (would you prefer if I weren/'t 4)
                       (perhaps I am 4 in your fantasies)
                       (do you sometimes think I am 4)
                       how)
          ((are 0) (nil) how)
          ((0 are 0) (nil)
                     (did you think they might not be 3)
                     (would you like it if they were not 3)
                     (what if they were not 3)
                     (possibly they are 3)))
         rules)

(defprop your 0 priority)

(defprop your my translation)

(defprop your
         (((0 my 0) (nil)
                    (why are you concerned over my 3)
                    (what about your own 3)
                    (are you worried about someone elses 3)
                    (really/, my 3)))
         rules)

(defprop was 2 priority)

(defprop was
         (((0 was you 0) (nil)
                         (what if you were 4)
                         (do you think you were 4)
                         (were you 4)
                         (what would it mean if you were 4)
                         (what does /' 4 /' suggest to you)
                         how)
          ((0 you was 0) (nil)
                         (were you really)
                         (why do you tell me you were 4 now)
                         (perhaps I already knew you were 4))
          ((0 was I 0) (nil)
                       (would you like to believe I was 4)
                       (what suggests that I was 4)
                       (what do you think)
                       (perhaps I was 4)
                       (what if I had been 4))
          ((0) (nil) newkey))
         rules)

(defprop were 0 priority)

(defprop were was translation)

(defprop were (was) rules)

(defprop me you translation)

(defprop you/'re 0 priority)

(defprop you/'re I/'m translation)

(defprop you/'re (((0 I/'m 0) (nil) (pre (I are 3) you))) rules)

(defprop I/'m 0 priority)

(defprop I/'m you/'re translation)

(defprop I/'m (((0 you/'re 0) (nil) (pre (you are 3) I))) rules)

(defprop myself yourself translation)

(defprop yourself myself translation)

(defprop mother t family)

(defprop mom mother translation)

(defprop mom t family)

(defprop mommy mother translation)

(defprop mommy t family)

(defprop dad father translation)

(defprop dad t family)

(defprop father t family)

(defprop daddy father translation)

(defprop daddy t family)

(defprop sister t family)

(defprop brother t family)

(defprop wife t family)

(defprop children t family)

(defprop I 0 priority)

(defprop I you translation)

(defprop I
         (((0 you (want need) 0)
           (nil)
           (what would it mean to you if you got 4)
           (why do you want 4)
           (suppose you got 4 soon)
           (what if you never got 4)
           (what would getting 4 mean to you)
           (what does wanting 4 have to do with this discussion)
           (you really want 4)
           (I suspect you really don/'t want 4))
          ((0 you are 0 (sad unhappy depressed sick) 0)
           (nil)
           (I am sorry to hear you are 5)
           (do you think coming here will help you not to be 5)
           (I/'m sure its not pleasant to be 5)
           (can you explain what made you 5)
           (please go on))
          ((0 you are 0 (happy elated glad better) 0)
           (nil)
           (how have I helped you to be 5)
           (has your treatment made you 5)
           (what makes you 5 just now)
           (can you explain why you are suddenly 5)
           (are you sure)
           (what do you mean by 5))
          ((0 you was 0) (nil) was)
          ((0 you (nil belief) you 0) (nil)
                                      (do you really think so)
                                      (but you are not sure you 5)
                                      (do you really doubt you 5))
          ((0 you 0 (nil belief) 0 I 0) (nil) you)
          ((0 you are 0) (nil)
                         (is it because you are 4 that you came to me)
                         (how long have you been 4)
                         (do you believe it normal to be 4)
                         (do you enjoy being 4))
          ((0 you (can/'t cannot) 0) (nil)
                                    (how do you know you can/'t 4)
                                    (have you tried)
                                    (perhaps you could 4 now)
                                    (do you really want to be able to 4))
          ((0 you don/'t 0) (nil)
                           (don/'t you really 4)
                           (why don/'t you 4)
                           (do you wish to be able to 4)
                           (does that trouble you))
          ((0 you feel 0) (nil)
                          (tell me more about such feelings)
                          (do you often feel 4)
                          (do you enjoy feeling 4)
                          (of what does feeling 4 remind you))
          ((0 you 0 I 0) (nil)
                         (perhaps in your fantasy we 3 each other)
                         (do you wish to 3 me)
                         (you seem to need to 3 me)
                         (do you 3 anyone else))
          ((0) (nil)
               (you say 1)
               (can you elaborate on that)
               (do you say 1 for some special reason)
               (that/'s quite interesting)))
         rules)

(defprop you 0 priority)

(defprop you I translation)

(defprop you
         (((0 I remind you of 0) (nil) dit)
          ((0 I are 0 you 0) (nil) newkey)
          ((0 I 0 are 0) (nil)
                         (what makes you think I am 5)
                         (does it please you to believe I am 5)
                         (perhaps you would like to be 5)
                         (do you sometimes wish you were 5))
          ((0 I 0 you) (nil)
                       (why do you think I 3 you)
                       (you like to think I 3 you /- don/'t you)
                       (what makes you think I 3 you)
                       (really/, I 3 you)
                       (do you wish to believe I 3 you)
                       (suppose I did 3 you /- what would that mean)
                       (does someone else believe I 3 you))
          ((0 I 1 0) (nil)
                     (we were discussing you /- not me)
                     (oh/, I 3 4)
                     (is this really relevant to your problem)
                     (perhaps I do 3 4)
                     (are you glad to know I 3 4)
                     (do you 3 4)
                     (what are your feelings now))
          ((0) (nil) newkey))
         rules)

(defprop we you translation)

(defprop we 0 priority)

(defprop we (I) rules)

(defprop xxyyzz 0 priority)

(defprop xxyyzz
         (((0) (nil)
               (youre being somewhat short with me)
               (you dont seem very talkative today)
               (perhaps youd rather talk about something else)
               (are you using monosyllables for some reason)
               newkey))
         rules)

(defprop yes 0 priority)

(defprop yes
         (((yes) (nil) xxyyzz (pre (x yes) yes)) ((0) (nil)
                                                      (you seem quite positive)
                                                      (you are sure)
                                                      (I see)
                                                      (I understand)
                                                      newkey))
         rules)

(defprop no 0 priority)

(defprop no
         (((no) (nil) xxyyzz (pre (x no) no))
          ((0) (nil)
               (are you saying /'no/' just to be negative)
               (you are being a bit negative)
               (why not)
               (why /'no/')
               newkey))
         rules)

(defprop my 2 priority)

(defprop my your translation)

(defprop my
         (((0 your 0 (nil family) 0)
           (nil)
           (tell me more about your family)
           (who else in your family 5)
           (your 4)
           (what else comes to mind when you think of your 4))
          ((0 your 1 0) (nil)
                        (your 3 4)
                        (why do you say your 3 4)
                        (does that suggest anything else which belongs to you)
                        (is it important to you that your 3 4))
          ((0) (nil) newkey))
         rules)

(defprop can 0 priority)

(defprop can
         (((0 can I 0) (nil)
                       (you believe I can 4 don/'t you)
                        how
                       (you want me to be able to 4)
                       (perhaps you would like to be able to 4 yourself))
          ((0 can you 0)
           (nil)
           (whether or not you can 4 depends on you more than on me)
           (do you want to be able to 4)
           (perhaps you don/'t want to 4)
           how)
         ((0) (nil) how newkey))
         rules)

(defprop is 0 priority)

(defprop is
         (((1 0 is 1 0) (nil)
                        (suppose 1 2 were not 4 5)
                        (perhaps 1 2 really is 4 5)
                        (tell me more about 1 2))
          ((0) (nil) newkey))
         rules)

(defprop where 0 priority)

(defprop where (how) rules)

(defprop how 0 priority)

(defprop how
         (((0) (nil)
               (why do you ask)
               (does that question interest you)
               (what is it you really want to know)
               (are such questions much on your mind)
               (what answer would please you most)
               (what do you think)
               (what comes to your mind when you ask that)
               (have you asked such questions before)
               (have you asked anyone else)))
         rules)

(defprop because 0 priority)

(defprop because
         (((0) (nil)
               (is that the real reason)
               (don/'t any other reasons come to mind)
               (does that reason seem to explain anything else)
               (what other reasons might there be)
               (you/'re not concealing anything from me/, are you)))
         rules)

(defprop why 0 priority)

(defprop why
         (((0 why don/'t I 0) (nil)
                             (do you believe I don/'t 5)
                             (perhaps I will 5 in good time)
                             (should you 5 yourself)
                             (you want me to 5)
                             how)
          ((0 why can/'t you 0) (nil)
                               (do you think you should be able to 5)
                               (do you want to be able to 5)
                               (do you believe this will help you to 5)
                               (have you any idea why you can/'t 5)
                               how)
         ((0) (nil) (why indeed) (why /'why/') (why not) how newkey))
         rules)

(defprop everyone 2 priority)

(defprop everyone
         (((0 (everyone everybody nobody noone) 0)
           (nil)
           (really/, 2)
           (surely not 2)
           (can you think of anyone in particular)
           (who/, for example)
           (you are thinking of a very special person)
           (who/, may I ask)
           (someone special perhaps)
           (you have a particular person in mind/, don/'t you)
           (who do you think you/'re talking about)
           (I suspect you/'re exaggerating a little)))
         rules)

(defprop everybody 2 priority)

(defprop everybody (everyone) rules)

(defprop nobody 2 priority)

(defprop nobody (everyone) rules)

(defprop noone 2 priority)

(defprop noone (everyone) rules)

(defprop always 1 priority)

(defprop always
         (((0) (nil)
               (can you think of a specific example)
               (when)
               (what incident are you thinking of)
               (really/, always)
               (what if this never happened)))
         rules)

(defprop like 12 priority)

(defprop like
         (((0 (am is are was) 0 like 0) (nil) dit) ((0) (nil) newkey))
         rules)

(defprop dit
         (((0) (nil)
               (in what way)
               (what resemblance do you see)
               (what does that similarity suggest to you)
               (what other connections do you see)
               (what do you suppose that resemblance means)
               (what is the connection/, do you suppose)
               (could there really be some connection)
               (how)))
         rules)

(defprop bag 5 priority)

(defprop bag
         (((0 (bite bites) the bag) (nil)
                                    (do you say 1 2 3 4 for some special reason)
                                    (what might 1 have to do with your problem)
                                    (do you often say /' 2 3 4 /')
                                    (perhaps you feel that you bite 3 4))
         ((0) (/il) (why the interest in bags) (please be more specific)
          (you aren/'t pulling my leg are you) newkey))
         rules)

(defprop zzyyxx (((0) (nil) newkey)) rules)

(defprop fuck 67 priority)

(defprop fuck
         (((0) (nil)
               (are such obscenities frequently on your mind)
               (you are being a bit childish)
               (really now)
               (dear me)
               (I really shouldn/'t tolerate such language)
               newkey))
         rules)

(defprop shit 67 priority)

(defprop shit (fuck) rules)

(defprop cunt 67 priority)

(defprop cunt (fuck) rules)

(defprop piss 67 priority)

(defprop piss (fuck) rules)

(defprop barf 0 priority)

(defprop barf (fuck) rules)

(defprop damn 0 priority)

(defprop damn (fuck) rules)

(defprop hell 0 priority)

(defprop hell (fuck) rules)

(defprop suck 5 priority)

(defprop suck (fuck) rules)

(defprop sucks suck translation)

(defprop problem 5 priority)

(defprop problem
         (((0 (is are) your (problem problems) 0) (nil)
                                                  (1 2 your 4)
                                                  (are you sure 1 2 your 4)
                                                  (perhaps 1 2 not your real 4)
                                                  (you think you have problems)
                                                  (do you often think about 1))
          ((0 your (problem problems) (is are) 0) (nil)
                                                  (your 2 3 4)
                                                  (are you sure your2 3 4)
                                                  (perhaps your real 2 3 not 4)
                                                  (you think you have problems))
          ((0) (nil)
               (please continue/, this may be interesting)
               (have you any other problems you wish to discuss)
               (perhaps you/'d rather change the subject)
               (you seem a bit uneasy)
               newkey))
         rules)

(defprop problems 5 priority)

(defprop problems (problem) rules)

(defprop problem
         (((0 is your problem 0)
           (nil)
           (earlier you mentioned 1)
           (let/'s talk further about 1)
           (tell me more about 1)
           (you haven/'t mentioned 1 for a while)))
         memr)

(defprop problems
        (((0 are you problems)
        (nil)
        (earlier you mentioned 1)
        (let/'s talk about 1)
        (tell me more about 1)
        (you haven/'t mentioned 1 for a while)))
        memr)

(defprop crap 6 priority)

(defprop crap (fuck) rules)

(defprop ask 0 priority)

(defprop ask
        (((0 you ask 0)
          (nil)
          how)
         ((0 you 1 asking 0)
          (nil)
          how)
         ((0 I 0)
          (nil)
          you)
         ((0)
          (nil)
          newkey))
        rules)


(defprop /10-4 0 priority)
(defprop /10-4 (zap) rules)

(defprop chickens 48. priority)

(defprop chicken 48. priority)

(defprop chicken (chickens) rules)

(defprop chickens
         ((((do Do) chickens have lips)
           (nil)
           (Yes they
                do/.
                Mother
                nature
                puts
                them
                behind
                their
                beaks/,
                to
                protect
                them
                from
                people
                with
                chicken
                fetishes/.)
           (It is not normal to want to know if 2. 3. 4.)
           (I am no longer interested in pursuing homosexuality)
           how)
          ((0.)
           (nil)
           (Do you frequently think about chickens?)
           (You seem obsessed with chickens)))
         rules)

(defprop loves 32. priority)

(defprop loves
         ((((Nobody nobody) loves you and your hands are cold)
           (nil)
           (God loves you and you can sit on your hands))
          ((0.) (nil) newkey))
         rules)

(defprop Multics 48. priority)

(defprop multics 48. priority)

(defprop Multics multics translation)

(defprop multics
         (((0.) (nil) computer))
         rules)


(defprop Multics (multics) rules)

(defprop  lisp 48. priority)
(defprop lisp (((you want 0 lisp)
                (nil)
                (eval (eval at your service)
                      (terpri)
                      (break doctor)))
               ((0) (nil) (Lisp is a fine language/. Why do you mention it?) newkey)) rules)

(defprop sign 12. priority)
(defprop sign (((what is my sign) (nil) (my sign is /'positive/'))
               ((0 your sign 0) (nil) (do you believe in reincarnation as well?))
               ((0) (nil) newkey))
         rules)
