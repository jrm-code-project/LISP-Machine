;;;-*- Mode:LISP; Package:NC; Base:10; Readtable:CL -*-


(defun li:list-length (list)
  (do ((n 0 (+ n 2))
       (y list (cddr y))
       (x list (cdr x)))
      (())
    (when (atom y) (return n))
    (when (atom (cdr y)) (return (1+ n)))
    (when (and (eq x y) (plusp n)) (return nil))))

LIST-LENGTH_16
   (MOVEI A3 (QUOTE 0) BOXED)
   (MOVE A2 A0)
   (MOVE A1 A0)
P_18
DO1051_25
   (ALU-FIELD FIELD-PASS R2 A2 (REGISTER *ZERO* 4 0) (BYTE 6 -26) PW-II DT-NONE BOXED-RIGHT)
   (ALU L-R NOP R2 (REGISTER *TWO* 4 3) BW-24 DT-BOTH-FIXNUM)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_37 NIL)
C_34
   (RETURN A3)
C_37
   (OPEN-CALL (CDR 1) R2 (O0 A2))
   (ALU-FIELD FIELD-PASS R2 R2 (REGISTER *ZERO* 4 0) (BYTE 6 -26) PW-II DT-NONE BOXED-RIGHT)
   (ALU L-R NOP R2 (REGISTER *TWO* 4 3) BW-24 DT-BOTH-FIXNUM)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_47 NIL)
C_44
   (ALU R+1 RETURN IGNORE A3 BW-24 DT-BOTH-FIXNUM-WITH-OVERFLOW CH-RETURN NEXT-PC-RETURN)
C_47
L_119
   (ALU L-R NOP A1 A2 BW-32 DT-NONE)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_62 NIL)
C_65
   (MOVE NOP A3 BW-24 DT-BOTH-FIXNUM)
   (TEST BR-NOT-POSITIVE)
   (BRANCH C_62 NIL)
C_59
   (RETURNI (QUOTE NIL))
C_62
   (ALU R+2 A3 IGNORE A3 BW-24 DT-BOTH-FIXNUM-WITH-OVERFLOW)
   (OPEN-CALL (CDDR 1) A2 (O0 A2))
   (OPEN-CALL (CDR 1) A1 (O0 A1))
   (UNCONDITIONAL-BRANCH DO1051_25 NIL)



;----------------------------------------------------

(defun li:length (seq)
;  (cond ((listp seq)
         (do ((l seq (cdr l))
              (i 0 (1+ i)))
             ((atom l) i)))

LENGTH_13
   (MOVE A2 A0)
   (MOVEI A1 (QUOTE 0) BOXED)
P_15
DO1055_22
   (ALU-FIELD FIELD-PASS R2 A2 (REGISTER *ZERO* 4 0) (BYTE 6 -26) PW-II DT-NONE BOXED-RIGHT)
   (ALU L-R NOP R2 (REGISTER *TWO* 4 3) BW-24 DT-BOTH-FIXNUM)
   (TEST BR-NOT-EQUAL)
   (BRANCH C_28 NIL)
C_25
   (RETURN A1)
C_28
   (OPEN-CALL (CDR 1) A2 (O0 A2))
   (UNCONDITIONAL-BRANCH DO1055_22 (ALU R+1 A1 IGNORE A1 BW-24 DT-BOTH-FIXNUM-WITH-OVERFLOW))







;---------------------------------------------------------


;;; DESTRU -- Destructive operation benchmark
(defun destructive (n m)
  (let ((l (do ((i 10. (1- i))
                (a () (push () a)))
               ((= i 0) a))))
    (do ((i n (1- i)))
        ((= i 0))
      (cond ((null (car l))
             (do ((l l (cdr l)))
                 ((null l))
               (or (car l)
                   (rplaca l (cons () ())))
               (nconc (car l)
                      (do ((j m (1- j))
                           (a () (push () a)))
                          ((= j 0) a)))))
            (t
             (do ((l1 l (cdr l1))
                  (l2 (cdr l) (cdr l2)))
                 ((null l2))
               (rplacd (do ((j (floor (length (car l2)) 2)
                               (1- j))
                            (a (car l2) (cdr a)))
                           ((zerop j) a)
                         (rplaca a i))
                       (let ((n (floor (length (car l1)) 2)))
                         (cond ((= n 0) (rplaca l1 ())
                                (car l1))
                               (t
                                (do ((j n (1- j))
                                     (a (car l1) (cdr a)))
                                    ((= j 1)
                                     (prog1 (cdr a)
                                            (rplacd a ())))
                                  (rplaca a i))))))))))))



Simplified tree:
10067246    ((T_39 NIL C_38) ($*DEFINE 1 ^B_299 DESTRUCTIVE ^P_40))   NIL
10090374     ((B_299 IGNORE_298) (C_38 0 (QUOTE T)))   NIL
10067381     ((P_40 NIL K_0 N_1 M_2) ($Y 1 ^Y_272))   NIL
10087851      ((Y_272 NIL C_270 DO8757_33) (C_270 0 ^C_271 ^T_274))   NIL
10087809       ((C_271 NIL NIL) ($OPEN 1 ^C_294 (QUOTE #{NC::CALL-NODE (DO8757_33 1 ^C_296 (QUOTE 10) (QUOTE NIL)) 10089866})))   NIL
10090008        ((C_294)  (DO8757_33 1 ^C_296 (QUOTE 10) (QUOTE NIL)))   NIL
10110887         ((C_296 NIL L_295) ($Y 1 ^Y_44))   NIL
10110944          ((Y_44 NIL C_42 DO8749_5) (C_42 0 ^C_43 ^T_46))   NIL
10111041           ((C_43 NIL NIL) ($OPEN 1 ^C_269 (QUOTE #{NC::CALL-NODE (DO8749_5 1 K_0 N_1) 10111113})))   NIL
10111096            ((C_269)  (DO8749_5 1 K_0 N_1))   NIL
10111378           ((T_46 NIL C_45) (C_45 0 ^DO8749_47))   NIL
10111344            ((DO8749_47 NIL K_6 I_7) ($CONDITIONAL 2 ^C_50 ^C_51 $= I_7 (QUOTE 0)))   NIL
10128437             ((C_50 NIL) (K_6 0 (QUOTE NIL)))   NIL
10111310             ((C_51 NIL) (^P_53 1 ^B_265))   NIL
10111763              ((P_53 NIL J_52) ($OPEN 1 ^C_254 (QUOTE #{NC::CALL-NODE (CAR 1 ^C_256 L_295) 10111712})))   NIL
10111729               ((C_254)  (CAR 1 ^C_256 L_295))   NIL
10111695                ((C_256 NIL V_255) ($CONDITIONAL 2 ^C_54 ^C_126 $EQ V_255 (QUOTE NIL)))   NIL
10111661                 ((C_54 NIL) ($Y 1 ^Y_57))   NIL
10111627                  ((Y_57 NIL C_55 DO8750_8) (C_55 0 ^C_56 ^T_59))   NIL
10111593                   ((C_56 NIL NIL) ($OPEN 1 ^C_125 (QUOTE #{NC::CALL-NODE (DO8750_8 1 J_52 L_295) 10111542})))   NIL
10111559                    ((C_125)  (DO8750_8 1 J_52 L_295))   NIL
10120094                   ((T_59 NIL C_58) (C_58 0 ^DO8750_60))   NIL
10119939                    ((DO8750_60 NIL K_9 L_10) ($CONDITIONAL 2 ^C_63 ^C_64 $EQ L_10 (QUOTE NIL)))   NIL
10120256                     ((C_63 NIL) (K_9 0 (QUOTE NIL)))   NIL
10120293                     ((C_64 NIL) ($OPEN 1 ^C_80 (QUOTE #{NC::CALL-NODE (^P_65 1 ^B_114) 10123944})))   NIL
10131207                      ((C_80)   ($OPEN 1 ^C_77 (QUOTE #{NC::CALL-NODE (CAR 1 ^C_79 L_10) 10120592})))   NIL
10120609                       ((C_77)   (CAR 1 ^C_79 L_10))   NIL
10115436                        ((C_79 NIL G8751_78) (^P_65 1 ^B_114))   NIL
10123964                         ((P_65 NIL K_11) ($CONDITIONAL 2 ^C_68 ^C_69 $TEST $TRUE? G8751_78))   NIL
10124079                          ((C_68 NIL) (K_11 0 G8751_78))   NIL
10120830                          ((C_69 NIL) ($OPEN 1 ^C_75 (QUOTE #{NC::CALL-NODE (RPLACA 1 K_11 L_10 V_73) 10120686})))   NIL
10131335                           ((C_75)   ($OPEN 1 ^C_72 (QUOTE #{NC::CALL-NODE (CONS 1 ^C_74 (QUOTE NIL) (QUOTE NIL)) 10120720})))   NIL
10131301                            ((C_72)   (CONS 1 ^C_74 (QUOTE NIL) (QUOTE NIL)))   NIL
10120703                             ((C_74 NIL V_73) (RPLACA 1 K_11 L_10 V_73))   NIL
10124378                         ((B_114 IGNORE_113) ($OPEN 1 ^C_112 (QUOTE #{NC::CALL-NODE (NCONC 1 ^B_121 V_108 V_110) 10119518})))   NIL
10125321                          ((C_112)  ($Y 1 ^Y_85))   NIL
10125287                           ((Y_85 NIL C_83 DO8752_13) (C_83 0 ^C_84 ^T_87))   NIL
10125624                            ((C_84 NIL NIL) ($OPEN 1 ^C_107 (QUOTE #{NC::CALL-NODE (DO8752_13 1 ^C_111 M_2 (QUOTE NIL)) 10125566})))   NIL
10125679                             ((C_107)  (DO8752_13 1 ^C_111 M_2 (QUOTE NIL)))   NIL
10130500                              ((C_111 NIL V_110) ($OPEN 1 ^C_82 (QUOTE #{NC::CALL-NODE (CAR 1 ^C_109 L_10) 10120498})))   NIL
10120515                               ((C_82)   (CAR 1 ^C_109 L_10))   NIL
10119535                                ((C_109 NIL V_108) (NCONC 1 ^B_121 V_108 V_110))   NIL
10119501                                 ((B_121 IGNORE_120) ($OPEN 1 ^C_119 (QUOTE #{NC::CALL-NODE (DO8750_8 1 K_9 V_117) 10119653})))   NIL
10119467                                  ((C_119)  ($OPEN 1 ^C_116 (QUOTE #{NC::CALL-NODE (CDR 1 ^C_118 L_10) 10119416})))   NIL
10119433                                   ((C_116)  (CDR 1 ^C_118 L_10))   NIL
10119670                                    ((C_118 NIL V_117) (DO8750_8 1 K_9 V_117))   NIL
10125253                            ((T_87 NIL C_86) (C_86 0 ^DO8752_88))   NIL
10125219                             ((DO8752_88 NIL K_14 J_15 A_16) ($CONDITIONAL 2 ^C_91 ^C_92 $= J_15 (QUOTE 0)))   NIL
10125881                              ((C_91 NIL) (K_14 0 A_16))   NIL
10125185                              ((C_92 NIL) ($OPEN 1 ^C_101 (QUOTE #{NC::CALL-NODE (DO8752_13 1 K_14 V_97 V_99) 10125603})))   NIL
10125151                               ((C_101)  ($OPEN 1 ^C_94 (QUOTE #{NC::CALL-NODE (CONS 1 ^C_96 (QUOTE NIL) A_16) 10124946})))   NIL
10124963                                ((C_94)   (CONS 1 ^C_96 (QUOTE NIL) A_16))   NIL
10126215                                 ((C_96 NIL V_95) ($SETQ-LEXICAL 1 ^C_100 A_16 V_95))   NIL
10126181                                  ((C_100 NIL V_99) ($1- 1 ^C_98 J_15))   NIL
10126301                                   ((C_98 NIL V_97) (DO8752_13 1 K_14 V_97 V_99))   NIL
10112029                 ((C_126 NIL) ($Y 1 ^Y_129))   NIL
10111995                  ((Y_129 NIL C_127 DO8753_17) (C_127 0 ^C_128 ^T_131))   NIL
10111961                   ((C_128 NIL NIL) ($OPEN 1 ^C_253 (QUOTE #{NC::CALL-NODE (DO8753_17 1 J_52 L_295 V_251) 10111842})))   NIL
10111927                    ((C_253)  ($OPEN 1 ^C_250 (QUOTE #{NC::CALL-NODE (CDR 1 ^C_252 L_295) 10111876})))   NIL
10111893                     ((C_250)  (CDR 1 ^C_252 L_295))   NIL
10111859                      ((C_252 NIL V_251) (DO8753_17 1 J_52 L_295 V_251))   NIL
10120115                   ((T_131 NIL C_130) (C_130 0 ^DO8753_132))   NIL
10120990                    ((DO8753_132 NIL K_18 L1_19 L2_20) ($CONDITIONAL 2 ^C_135 ^C_136 $EQ L2_20 (QUOTE NIL)))   NIL
10121207                     ((C_135 NIL) (K_18 0 (QUOTE NIL)))   NIL
10121371                     ((C_136 NIL) ($OPEN 1 ^C_237 (QUOTE #{NC::CALL-NODE (RPLACD 1 ^B_246 V_233 V_235) 10112415})))   NIL
10125541                      ((C_237)  ($OPEN 1 ^C_232 (QUOTE #{NC::CALL-NODE (^P_176 1 ^C_236) 10116758})))   NIL
10116922                       ((C_232)  ($OPEN 1 ^C_229 (QUOTE #{NC::CALL-NODE (FLOOR 1 ^C_231 V_227 (QUOTE 2)) 10116435})))   NIL
10116654                        ((C_229)  ($OPEN 1 ^C_226 (QUOTE #{NC::CALL-NODE (LENGTH 1 ^C_228 V_224) 10116507})))   NIL
10125014                         ((C_226)  ($OPEN 1 ^C_223 (QUOTE #{NC::CALL-NODE (CAR 1 ^C_225 L1_19) 10116562})))   NIL
10124980                          ((C_223)  (CAR 1 ^C_225 L1_19))   NIL
10116545                           ((C_225 NIL V_224) (LENGTH 1 ^C_228 V_224))   NIL
10116452                            ((C_228 NIL V_227) (FLOOR 1 ^C_231 V_227 (QUOTE 2)))   NIL
10116741                             ((C_231 NIL N_230) (^P_176 1 ^C_236))   NIL
10125521                              ((P_176 NIL K_25) ($CONDITIONAL 2 ^C_179 ^C_184 $= N_230 (QUOTE 0)))   NIL
10122285                               ((C_179 NIL) ($OPEN 1 ^C_180 (QUOTE #{NC::CALL-NODE (RPLACA 1 ^B_183 L1_19 (QUOTE NIL)) 10122234})))   NIL
10122251                                ((C_180)  (RPLACA 1 ^B_183 L1_19 (QUOTE NIL)))   NIL
10122087                                 ((B_183 IGNORE_182) ($OPEN 1 ^C_181 (QUOTE #{NC::CALL-NODE (CAR 1 K_25 L1_19) 10122015})))   NIL
10122032                                  ((C_181)  (CAR 1 K_25 L1_19))   NIL
10126580                               ((C_184 NIL) ($Y 1 ^Y_187))   NIL
10126511                                ((Y_187 NIL C_185 DO8755_27) (C_185 0 ^C_186 ^T_189))   NIL
10126694                                 ((C_186 NIL NIL) ($OPEN 1 ^C_219 (QUOTE #{NC::CALL-NODE (DO8755_27 1 K_25 N_230 V_217) 10121976})))   NIL
10121897                                  ((C_219)  ($OPEN 1 ^C_216 (QUOTE #{NC::CALL-NODE (CAR 1 ^C_218 L1_19) 10121846})))   NIL
10121863                                   ((C_216)  (CAR 1 ^C_218 L1_19))   NIL
10121959                                    ((C_218 NIL V_217) (DO8755_27 1 K_25 N_230 V_217))   NIL
10126711                                 ((T_189 NIL C_188) (C_188 0 ^DO8755_190))   NIL
10117595                                  ((DO8755_190 NIL K_28 J_29 A_30) ($CONDITIONAL 2 ^C_193 ^C_202 $= J_29 (QUOTE 1)))   NIL
10127401                                   ((C_193 NIL) ($OPEN 1 ^C_198 (QUOTE #{NC::CALL-NODE (CDR 1 ^C_200 A_30) 10117979})))   NIL
10117996                                    ((C_198)  (CDR 1 ^C_200 A_30))   NIL
10118053                                     ((C_200 NIL G8756_199) ($OPEN 1 ^C_195 (QUOTE #{NC::CALL-NODE (RPLACD 1 ^B_197 A_30 (QUOTE NIL)) 10118090})))   NIL
10118107                                      ((C_195)  (RPLACD 1 ^B_197 A_30 (QUOTE NIL)))   NIL
10118165                                       ((B_197 IGNORE_196) (K_28 0 G8756_199))   NIL
10122451                                   ((C_202 NIL) ($OPEN 1 ^C_203 (QUOTE #{NC::CALL-NODE (RPLACA 1 ^B_212 A_30 I_7) 10116247})))   NIL
10116264                                    ((C_203)  (RPLACA 1 ^B_212 A_30 I_7))   NIL
10117143                                     ((B_212 IGNORE_211) ($OPEN 1 ^C_210 (QUOTE #{NC::CALL-NODE (DO8755_27 1 K_28 V_206 V_208) 10117239})))   NIL
10117109                                      ((C_210)  ($OPEN 1 ^C_205 (QUOTE #{NC::CALL-NODE (CDR 1 ^C_209 A_30) 10117058})))   NIL
10117075                                       ((C_205)  (CDR 1 ^C_209 A_30))   NIL
10117505                                        ((C_209 NIL V_208) ($1- 1 ^C_207 J_29))   NIL
10117256                                         ((C_207 NIL V_206) (DO8755_27 1 K_28 V_206 V_208))   NIL
10128798                              ((C_236 NIL V_235) ($Y 1 ^Y_140))   NIL
10128707                               ((Y_140 NIL C_138 DO8754_21) (C_138 0 ^C_139 ^T_142))   NIL
10122110                                ((C_139 NIL NIL) ($OPEN 1 ^C_175 (QUOTE #{NC::CALL-NODE (DO8754_21 1 ^C_234 V_173 V_171) 10112449})))   NIL
10112814                                 ((C_175)  ($OPEN 1 ^C_169 (QUOTE #{NC::CALL-NODE (FLOOR 1 ^C_174 V_167 (QUOTE 2)) 10112551})))   NIL
10112778                                  ((C_169)  ($OPEN 1 ^C_166 (QUOTE #{NC::CALL-NODE (LENGTH 1 ^C_168 V_164) 10112602})))   NIL
10112704                                   ((C_166)  ($OPEN 1 ^C_163 (QUOTE #{NC::CALL-NODE (CAR 1 ^C_165 L2_20) 10112653})))   NIL
10112670                                    ((C_163)  (CAR 1 ^C_165 L2_20))   NIL
10112636                                     ((C_165 NIL V_164) (LENGTH 1 ^C_168 V_164))   NIL
10112585                                      ((C_168 NIL V_167) (FLOOR 1 ^C_174 V_167 (QUOTE 2)))   NIL
10112534                                       ((C_174 NIL V_173) ($OPEN 1 ^C_170 (QUOTE #{NC::CALL-NODE (CAR 1 ^C_172 L2_20) 10112483})))   NIL
10112500                                        ((C_170)  (CAR 1 ^C_172 L2_20))   NIL
10112466                                         ((C_172 NIL V_171) (DO8754_21 1 ^C_234 V_173 V_171))   NIL
10112432                                          ((C_234 NIL V_233) (RPLACD 1 ^B_246 V_233 V_235))   NIL
10112398                                           ((B_246 IGNORE_245) ($OPEN 1 ^C_244 (QUOTE #{NC::CALL-NODE (DO8753_17 1 K_18 V_242 V_240) 10112211})))   NIL
10112364                                            ((C_244)  ($OPEN 1 ^C_238 (QUOTE #{NC::CALL-NODE (CDR 1 ^C_243 L1_19) 10112313})))   NIL
10112330                                             ((C_238)  (CDR 1 ^C_243 L1_19))   NIL
10112296                                              ((C_243 NIL V_242) ($OPEN 1 ^C_239 (QUOTE #{NC::CALL-NODE (CDR 1 ^C_241 L2_20) 10112245})))   NIL
10112262                                               ((C_239)  (CDR 1 ^C_241 L2_20))   NIL
10112228                                                ((C_241 NIL V_240) (DO8753_17 1 K_18 V_242 V_240))   NIL
10128917                                ((T_142 NIL C_141) (C_141 0 ^DO8754_143))   NIL
10118884                                 ((DO8754_143 NIL K_22 J_23 A_24) ($CONDITIONAL 2 ^C_146 ^C_147 $= J_23 (QUOTE 0)))   NIL
10119188                                  ((C_146 NIL) (K_22 0 A_24))   NIL
10128550                                  ((C_147 NIL) ($OPEN 1 ^C_148 (QUOTE #{NC::CALL-NODE (RPLACA 1 ^B_157 A_24 I_7) 10118434})))   NIL
10118460                                   ((C_148)  (RPLACA 1 ^B_157 A_24 I_7))   NIL
10118382                                    ((B_157 IGNORE_156) ($OPEN 1 ^C_155 (QUOTE #{NC::CALL-NODE (DO8754_21 1 K_22 V_151 V_153) 10118578})))   NIL
10118289                                     ((C_155)  ($OPEN 1 ^C_150 (QUOTE #{NC::CALL-NODE (CDR 1 ^C_154 A_24) 10118221})))   NIL
10118238                                      ((C_150)  (CDR 1 ^C_154 A_24))   NIL
10118700                                       ((C_154 NIL V_153) ($1- 1 ^C_152 J_23))   NIL
10118595                                        ((C_152 NIL V_151) (DO8754_21 1 K_22 V_151 V_153))   NIL
10111276              ((B_265 IGNORE_264) ($OPEN 1 ^C_263 (QUOTE #{NC::CALL-NODE (DO8749_5 1 K_6 V_261) 10111191})))   NIL
10111242               ((C_263)  ($1- 1 ^C_262 I_7))   NIL
10111208                ((C_262 NIL V_261) (DO8749_5 1 K_6 V_261))   NIL
10110451       ((T_274 NIL C_273) (C_273 0 ^DO8757_275))   NIL
10110526        ((DO8757_275 NIL K_34 I_35 A_36) ($CONDITIONAL 2 ^C_278 ^C_279 $= I_35 (QUOTE 0)))   NIL
10110584         ((C_278 NIL) (K_34 0 A_36))   NIL
10110865         ((C_279 NIL) ($OPEN 1 ^C_288 (QUOTE #{NC::CALL-NODE (DO8757_33 1 K_34 V_284 V_286) 10110678})))   NIL
10110831          ((C_288)  ($OPEN 1 ^C_281 (QUOTE #{NC::CALL-NODE (CONS 1 ^C_283 (QUOTE NIL) A_36) 10110780})))   NIL
10110797           ((C_281)  (CONS 1 ^C_283 (QUOTE NIL) A_36))   NIL
10110763            ((C_283 NIL V_282) ($SETQ-LEXICAL 1 ^C_287 A_36 V_282))   NIL
10110729             ((C_287 NIL V_286) ($1- 1 ^C_285 I_35))   NIL
10110695              ((C_285 NIL V_284) (DO8757_33 1 K_34 V_284 V_286))   NIL

Register preference classes:
(A*  V_286 V_282 L_295 A_36)
(*  V_284 I_35)
(A*  V_153 V_233 A_24 V_171)
(*  V_151 J_23 V_173)
(16  V_167)
(16  V_164)
(IGNORE  DO8869_21)
(A*  G8871_199)
(A*  V_208 A_30 V_217)
(IGNORE  DO8870_27)
(IGNORE  K_25)
(*  V_206 J_29)
(*  N_230)
(16  V_227)
(16  V_224)
(A*  V_240 L2_20 V_251)
(IGNORE  DO8868_17)
(A*  V_99 V_95 V_110 A_16)
(*  V_97 J_15)
(16  V_108)
(IGNORE  DO8867_13)
(17  V_73)
(IGNORE  K_11)
(*  G8866_78)
(IGNORE  DO8865_8)
(*  V_255)
(IGNORE  J_52)
(A*  V_261 I_7)
(IGNORE  DO8864_5)
(A*  V_242 L1_19)
(A*  V_117 L_10)
(IGNORE  DO8872_33)
(1  M_2)
(0  N_1)

V_286: 2
V_282: 2
L_295: 2
A_36: 2
V_284: 3
I_35: 3
V_153: 4
V_233: 4
A_24: 4
V_171: 4
V_151: 5
J_23: 5
V_173: 5
V_167: 16
V_164: 16
DO8869_21: IGNORE
G8871_199: 6
V_208: 7
A_30: 7
V_217: 7
DO8870_27: IGNORE
K_25: IGNORE
V_206: 8
J_29: 8
N_230: 9
V_227: 16
V_224: 16
V_240: 10
L2_20: 10
V_251: 10
DO8868_17: IGNORE
V_99: 11
V_95: 11
V_110: 11
A_16: 11
V_97: 12
J_15: 12
V_108: 16
DO8867_13: IGNORE
V_73: 17
K_11: IGNORE
G8866_78: 13
DO8865_8: IGNORE
V_255: 14
J_52: IGNORE
V_261: 15
I_7: 15
DO8864_5: IGNORE
V_242: 100
L1_19: 100
V_117: 101
L_10: 101
DO8872_33: IGNORE
M_2: 1
N_1: 0

(DEFKFUN DESTRUCTIVE (N M)
  TAG::P_40
         (MOVE A3 '10)
         (MOVE A2 'NIL)
  TAG::DO8881_275
         (ALU L-R GARBAGE A3 '0)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_279)
  TAG::C_278
  TAG::C_296
         (MOVE A15 A0)
  TAG::DO8873_47
         (ALU L-R GARBAGE A15 '0)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_51)
  TAG::C_50
         (MOVE RETURN 'NIL CH-RETURN)
  TAG::C_51
         (OPEN-CALL KWRAP::CAR '1 A14 (O0 A2))
  TAG::C_256
         (ALU L-R GARBAGE A14 'NIL)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_126)
  TAG::C_54
         (MOVE NIL A2)
  TAG::DO8874_60
         (ALU L-R GARBAGE NIL 'NIL)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_64)
  TAG::C_63
         (MOVE RETURN 'NIL CH-RETURN)
  TAG::C_64
         (OPEN-CALL KWRAP::CAR '1 A13 (O0 NIL))
  TAG::C_79
         (ALU L-R GARBAGE A13 'NIL)
         (TEST BR-EQUAL)
         (BRANCH TAG::C_69)
  TAG::C_68
  TAG::B_114
         (MOVE A12 A1 CH-OPEN)
         (MOVE A11 'NIL)
  TAG::DO8876_88
         (ALU L-R GARBAGE A12 '0)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_92)
  TAG::C_91
  TAG::C_111
         (OPEN-CALL KWRAP::CAR '1 O0 (O0 NIL))
  TAG::C_109
         (KCALL NCONC '2 NIL (O1 A11))
  TAG::B_121
         (OPEN-CALL KWRAP::CDR '1 NIL (O0 NIL))
  TAG::C_118
         (JUMP TAG::DO8874_60)
  TAG::C_92
         (MOVE O0 'NIL CH-OPEN)
         (KCALL KWRAP::CONS '2 A11 (O1 A11))
  TAG::C_96
         (ALU L+R-1 A12 A12 '0)
         (JUMP TAG::DO8876_88)
  TAG::C_69
         (KOPEN)
         (MOVE O0 'NIL CH-OPEN)
         (KCALL KWRAP::CONS '2 O1 (O1 'NIL))
  TAG::C_74
         (KCALL RPLACA '2 NIL (O0 NIL))
  TAG::C_418
         (JUMP TAG::B_114)
  TAG::C_126
         (OPEN-CALL KWRAP::CDR '1 A10 (O0 A2))
  TAG::C_252
         (MOVE NIL A2)
  TAG::DO8877_132
         (ALU L-R GARBAGE A10 'NIL)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_136)
  TAG::C_135
         (MOVE RETURN 'NIL CH-RETURN)
  TAG::C_136
         (KOPEN)
         (KOPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CAR '1 O0 (O0 NIL))
  TAG::C_225
         (KCALL LENGTH '1 O0 NIL)
  TAG::C_228
         (KCALL FLOOR '2 A9 (O1 '2))
  TAG::C_231
         (ALU L-R GARBAGE A9 '0)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_184)
  TAG::C_179
         (MOVE O0 NIL CH-OPEN)
         (KCALL RPLACA '2 NIL (O1 'NIL))
  TAG::B_183
         (OPEN-CALL KWRAP::CAR '1 NIL (O0 NIL))
  TAG::C_236
         (KOPEN)
         (KOPEN)
         (OPEN-CALL KWRAP::CAR '1 O0 (O0 A10))
  TAG::C_165
         (KCALL LENGTH '1 O0 NIL)
  TAG::C_168
         (KCALL FLOOR '2 A5 (O1 '2))
  TAG::C_174
         (OPEN-CALL KWRAP::CAR '1 A4 (O0 A10))
  TAG::C_172
  TAG::DO8878_143
         (ALU L-R GARBAGE A5 '0)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_147)
  TAG::C_146
  TAG::C_234
         (MOVE O0 A4)
         (KCALL RPLACD '2 NIL (O1 NIL))
  TAG::B_246
         (OPEN-CALL KWRAP::CDR '1 NIL (O0 NIL))
  TAG::C_243
         (OPEN-CALL KWRAP::CDR '1 A10 (O0 A10))
  TAG::C_241
         (JUMP TAG::DO8877_132)
  TAG::C_147
         (MOVE O0 A4 CH-OPEN)
         (KCALL RPLACA '2 NIL (O1 A15))
  TAG::B_157
         (OPEN-CALL KWRAP::CDR '1 A4 (O0 A4))
  TAG::C_154
         (ALU L+R-1 A5 A5 '0)
         (JUMP TAG::DO8878_143)
  TAG::C_184
         (OPEN-CALL KWRAP::CAR '1 A7 (O0 NIL))
  TAG::C_218
         (MOVE A8 A9)
  TAG::DO8879_190
         (ALU L-R GARBAGE A8 '1)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_202)
  TAG::C_193
         (OPEN-CALL KWRAP::CDR '1 A6 (O0 A7))
  TAG::C_200
         (MOVE O0 A7 CH-OPEN)
         (KCALL RPLACD '2 NIL (O1 'NIL))
  TAG::B_197
         (MOVE RETURN A6 CH-RETURN)
  TAG::C_202
         (MOVE O0 A7 CH-OPEN)
         (KCALL RPLACA '2 NIL (O1 A15))
  TAG::B_212
         (OPEN-CALL KWRAP::CDR '1 A7 (O0 A7))
  TAG::C_209
         (ALU L+R-1 A8 A8 '0)
         (JUMP TAG::DO8879_190)
  TAG::C_279
         (MOVE O0 'NIL CH-OPEN)
         (KCALL KWRAP::CONS '2 A2 (O1 A2))
  TAG::C_283
         (ALU L+R-1 A3 A3 '0)
         (JUMP TAG::DO8881_275))



;;; C_135



(defun destructive (n m)
  (let ((l (do ((i 10. (1- i))
                (a () (push () a)))
               ((= i 0) a))))
    (print l)))

((Y_18 NIL C_16 DO5956_5) (C_16 0 ^C_17 ^DO5956_21))   STRATEGY/LABEL
 ((C_17 NIL NIL) ($OPEN 1 ^C_41 DO5956_5))             STRATEGY/OPEN
  ((C_41)   (DO5956_5 1 ^C_43 (QUOTE 10) (QUOTE NIL))) STRATEGY/OPEN
   ((C_43 NIL L_42) ($OPEN 1 ^C_15 PRINT))             STRATEGY/OPEN
    ((C_15)   (PRINT 1 K_0 L_42))                      STRATEGY/OPEN
 ((DO5956_21 NIL K_6 I_7 A_8) ($CONDITIONAL 2 ^C_24 ^C_25 $= I_7 (QUOTE 0)))   STRATEGY/LABEL

can we say that when a is allocated to l we mark a2 as used
in scope of a?  we can only do that if we known allocation of l
what if we don't???
  body of a labels is allocated before label procs...

(let ((l -init-form-))
  -body-)

the lambda that binds l is the continuation of -init-form-
its body is -body-

in what cases is a var allocated to another var?
    1.   (setq other-var var)
    2.   ($primop ^c var literal-or-var-again ...)
           ((c other-var) ...)
    3.   ((lambda (other-var) ...) var)
    4.   (labels ((foo (other-var) ...))
           (foo var))

in what cases is other-var not yet allocated?

alloc to var
  if var is already alloced, set us to it also
     else get a reg for us and set other var to it???
          doesn't work because other var may get targeted
          to something which would be better for var


alloc x to y
alloc y to O-reg

(let ((x (bar)))
  (setq y x)
  (foo y))

(foo v2:(setq x v1:(hair)))

       (hair ^c1)
((c1 v1)(setq ^c2 x v1))
 ((c2 v2) (foo <cont> v2))

v1 targeted to x
x targeted to v2
v2 targeted to O0


okay, how about recursing then
 if var is targeted to other-var
    if other-var allocated
       use it
     else
       alloc other-var?

what about open-frame?


scope of var must be within open...
variable binder must be within open

a ref is only targeted to an o-reg if the parent
of the ref is the open call at
the time the binder of the ref is called

when a var is alloced to an open frame
 every ref must occur with the same open


((x a) ...)     open = foo



(defun foo ()
  (bar (do ((i 0 (1+ i)))
           ((= 1 10.) i))))


(DEFKFUN FOO NIL
  TAG::P_7
         (MOVE A0 '0 CH-TAIL-OPEN)
  TAG::DO7124_14
         (ALU L-R GARBAGE '1 '10)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_18)
  TAG::C_17
         (MOVE O0 A0)             ;this could go into next inst if c_30 tag not generated
  TAG::C_30                       ;why does i not get allocated to O0?
         (TAIL-CALL BAR '1)
  TAG::C_18
         (ALU L+R+1 A0 A0 '0)
         (JUMP TAG::DO7124_14))











IGNORE_45: IGNORE
N_1: 0
M_2: 1
L_42: 2
I_7: V_31
A_8: L_42
V_29: A_8
V_33: A_8
V_31: 2

Generating:
11924416    ((T_11 NIL C_10) ($*DEFINE 1 ^B_46 DESTRUCTIVE ^P_12))   STRATEGY/HEAP
11927515     ((B_46 IGNORE_45) (C_10 0 (QUOTE T)))   STRATEGY/OPEN
11924551     ((P_12 NIL K_0 N_1 M_2) ($Y 1 ^Y_18))   STRATEGY/HEAP
11924974      ((Y_18 NIL C_16 DO5956_5) (C_16 0 ^C_17 ^DO5956_21))   STRATEGY/LABEL
11924932       ((C_17 NIL NIL) ($OPEN 1 ^C_41 (QUOTE #{NC::CALL-NODE (DO5956_5 1 ^C_43 (QUOTE 10) (QUOTE NIL)) 11927007})))   STRATEGY/OPEN
11927149        ((C_41)   (DO5956_5 1 ^C_43 (QUOTE 10) (QUOTE NIL)))   STRATEGY/OPEN
11927273         ((C_43 NIL L_42) ($OPEN 1 ^C_15 (QUOTE #{NC::CALL-NODE (PRINT 1 K_0 L_42) 11924654})))   STRATEGY/OPEN
11924785          ((C_15)   (PRINT 1 K_0 L_42))   STRATEGY/OPEN
11925102       ((DO5956_21 NIL K_6 I_7 A_8) ($CONDITIONAL 2 ^C_24 ^C_25 $= I_7 (QUOTE 0)))   STRATEGY/LABEL
11925220        ((C_24 NIL) (K_6 0 A_8))   STRATEGY/LABEL
11925320        ((C_25 NIL) ($OPEN 1 ^C_35 (QUOTE #{NC::CALL-NODE (DO5956_5 1 K_6 V_31 V_33) 11925396})))   STRATEGY/LABEL
11926268         ((C_35)   ($OPEN 1 ^C_28 (QUOTE #{NC::CALL-NODE (CONS 1 ^C_30 (QUOTE NIL) A_8) 11925747})))   STRATEGY/OPEN
11925910          ((C_28)   (CONS 1 ^C_30 (QUOTE NIL) A_8))   STRATEGY/OPEN
11926017           ((C_30 NIL V_29) ($SETQ-LEXICAL 1 ^C_34 A_8 V_29))   STRATEGY/OPEN
11926189            ((C_34 NIL V_33) ($1- 1 ^C_32 I_7))   STRATEGY/OPEN
11926110             ((C_32 NIL V_31) (DO5956_5 1 K_6 V_31 V_33))   STRATEGY/OPEN
T_11
  (MOVE RETURN (QUOTE T))
  (RETURN)
P_12
  (MOVE A2 (QUOTE 10))
  (MOVE A2 (QUOTE NIL))
DO5956_21
  (ALU L-R GARBAGE A2 (QUOTE 0))
  (TEST BR-NOT-EQUAL)
  (BRANCH C_25)
C_24
C_43
  (TAIL-OPEN)
  (MOVE O0 A2)
  (TAIL-CALL PRINT (QUOTE 1))
C_25
  (KOPEN)
  (MOVE O0 (QUOTE NIL))
  (MOVE O1 A2)
  (KCALL CONS (QUOTE 2) A2)
C_30
  (ALU L+R-1 A2 A2 (QUOTE 0))
  (JUMP DO5956_21)
T_11
   (MOVE RETURN (QUOTE T) CH-RETURN)
P_12
   (MOVE A2 (QUOTE 10))
   (MOVE A2 (QUOTE NIL))
DO5956_21
   (ALU L-R GARBAGE A2 (QUOTE 0))
   (TEST BR-NOT-EQUAL)
   (BRANCH C_25)
C_24
C_43
   (OPEN-TAIL-CALL PRINT (QUOTE 1) (O0 A2))
C_25
   (MOVE O0 (QUOTE NIL) CH-OPEN)
   (KCALL CONS (QUOTE 2) A2 (O1 A2))
C_30
   (ALU L+R-1 A2 A2 (QUOTE 0))
   (JUMP DO5956_21)

(DEFKFUN DESTRUCTIVE
         (N M)
         TAG::P_12
         (MOVE A2 '10)
         (MOVE A2 'NIL)
         TAG::DO5954_21
         (ALU L-R GARBAGE A2 '0)
         (TEST BR-NOT-EQUAL)
         (BRANCH TAG::C_25)
         TAG::C_24
         TAG::C_43
         (OPEN-TAIL-CALL PRINT '1 (O0 A2))
         TAG::C_25
         (MOVE O0 'NIL CH-OPEN)
         (KCALL KWRAP::CONS '2 A2 (O1 A2))
         TAG::C_30
         (ALU L+R-1 A2 A2 '0)
         (JUMP TAG::DO5954_21))





(defun des (n m)
  (let ((l (foo n m)))
    (print l)))

(DEFKFUN DES
         (N M)
         TAG::P_8
         (MOVE O0 A0 CH-OPEN)
         (KCALL FOO '2 A2 (O1 A1))
         TAG::C_14
         (OPEN-TAIL-CALL PRINT '1 (O0 A2)))



(defun foo (a)
  (1+ ))




(defun d (a x)
  (setq a (cons nil x)))


;;; this is ok because l is live in do because l is live in label body
;;; anything live in any label fun (or body) is live in all
(defun dd ()
  (let ((l (foo)))
    (do ((i l (1+ i)))
        ((= i 10))
      (print i))
    (bar l)))


;;; happens here too, but here l and i can be in same var
(defun dd ()
  (let ((l (foo)))
    (do ((i l (1+ i)))
        ((= i 10))
      (print i))))

(BLOCK NIL
  (LABELS ((#:DO0237 (I)
             (IF (= I 10)
                 (PROGN)
               (PROGN (TAGBODY
                          (PRINT I))
                      (#:DO0237 (1+ I))))))
    (#:DO0237 L)))  ;l is live in body
