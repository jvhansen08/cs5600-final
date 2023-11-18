(in-package :user)

(defstruct op "An GPS operator"
  (action nil) 
  (preconds nil) 
  (add-list nil) 
  (del-list nil))

(defun executing-p (x)
  "Is x of the form: (execute ...) ?"
  (starts-with x 'execute))

(defun convert-op (op)
  "Make op conform to the (EXECUTING op) convention."
  (unless (some #'executing-p (op-add-list op))
    (push (list 'execute (op-action op)) (op-add-list op)))
  op)

(defun op (action &key preconds add-list del-list)
  "Make a new operator that obeys the (EXECUTING op) convention."
  (convert-op
    (make-op :action action :preconds preconds
             :add-list add-list :del-list del-list)))
(defparameter *registration-world* '())
(defparameter *registration-ops*
  (list

		(make-op :action 'take-acct-2010
			:add-list '(acct-2010))
		(make-op :action 'take-acct-2020
			:preconds '(acct-2010)
			:add-list '(acct-2020))
		(make-op :action 'take-acct-3110
			:preconds '(acct-2010 acct-2020)
			:add-list '(acct-3110))
		(make-op :action 'take-acct-3120
			:preconds '(acct-3110)
			:add-list '(acct-3120))
		(make-op :action 'take-acct-3310
			:preconds '(acct-2010 acct-2020)
			:add-list '(acct-3310))
		(make-op :action 'take-acct-3510
			:preconds '(acct-2010)
			:add-list '(acct-3510))
		(make-op :action 'take-acct-3520
			:preconds '(acct-3510)
			:add-list '(acct-3520))
		(make-op :action 'take-acct-4250
			:add-list '(acct-4250))
		(make-op :action 'take-acct-4500
			:preconds '(acct-3110)
			:add-list '(acct-4500))
		(make-op :action 'take-acct-4510
			:preconds '(acct-3110)
			:add-list '(acct-4510))
		(make-op :action 'take-acct-4700
			:add-list '(acct-4700))
		(make-op :action 'take-acct-4900
			:add-list '(acct-4900))
		(make-op :action 'take-acct-4950
			:add-list '(acct-4950))
		(make-op :action 'take-as-1010
			:preconds '(as-1110)
			:add-list '(as-1010))
		(make-op :action 'take-as-1020
			:preconds '(as-1120)
			:add-list '(as-1020))
		(make-op :action 'take-as-1110
			:preconds '(as-1010)
			:add-list '(as-1110))
		(make-op :action 'take-as-1120
			:preconds '(as-1020)
			:add-list '(as-1120))
		(make-op :action 'take-as-2010
			:preconds '(as-2110)
			:add-list '(as-2010))
		(make-op :action 'take-as-2020
			:preconds '(as-2120)
			:add-list '(as-2020))
		(make-op :action 'take-as-2110
			:preconds '(as-2010)
			:add-list '(as-2110))
		(make-op :action 'take-as-2120
			:preconds '(as-2020)
			:add-list '(as-2120))
		(make-op :action 'take-as-3010
			:preconds '(as-2010 as-2020 as-3110)
			:add-list '(as-3010))
		(make-op :action 'take-as-3020
			:preconds '(as-2010 as-2020 as-3120)
			:add-list '(as-3020))
		(make-op :action 'take-as-3060
			:add-list '(as-3060))
		(make-op :action 'take-as-3110
			:preconds '(as-3010)
			:add-list '(as-3110))
		(make-op :action 'take-as-3120
			:preconds '(as-3020)
			:add-list '(as-3120))
		(make-op :action 'take-as-4010
			:preconds '(as-3010 as-3020 as-4110)
			:add-list '(as-4010))
		(make-op :action 'take-as-4020
			:preconds '(as-3010 as-3020 as-4120)
			:add-list '(as-4020))
		(make-op :action 'take-as-4110
			:preconds '(as-4010)
			:add-list '(as-4110))
		(make-op :action 'take-as-4120
			:preconds '(as-4020)
			:add-list '(as-4120))
		(make-op :action 'take-ag-4250
			:add-list '(ag-4250))
		(make-op :action 'take-ag-4900
			:add-list '(ag-4900))
		(make-op :action 'take-aste-1010
			:add-list '(aste-1010))
		(make-op :action 'take-aste-1120
			:add-list '(aste-1120))
		(make-op :action 'take-aste-1130
			:add-list '(aste-1130))
		(make-op :action 'take-aste-1610
			:add-list '(aste-1610))
		(make-op :action 'take-aste-1615
			:preconds '(aste-1610)
			:add-list '(aste-1615))
		(make-op :action 'take-aste-1620
			:add-list '(aste-1620))
		(make-op :action 'take-aste-1625
			:preconds '(aste-1620)
			:add-list '(aste-1625))
		(make-op :action 'take-aste-1710
			:add-list '(aste-1710))
		(make-op :action 'take-aste-2100
			:add-list '(aste-2100))
		(make-op :action 'take-aste-2200
			:add-list '(aste-2200))
		(make-op :action 'take-aste-2250
			:add-list '(aste-2250))
		(make-op :action 'take-aste-2450
			:add-list '(aste-2450))
		(make-op :action 'take-aste-2500
			:add-list '(aste-2500))
		(make-op :action 'take-aste-2710
			:add-list '(aste-2710))
		(make-op :action 'take-aste-2900
			:add-list '(aste-2900))
		(make-op :action 'take-aste-2930
			:add-list '(aste-2930))
		(make-op :action 'take-aste-3030
			:add-list '(aste-3030))
		(make-op :action 'take-aste-3040
			:add-list '(aste-3040))
		(make-op :action 'take-aste-3050
			:preconds '(jcom-3050)
			:add-list '(aste-3050))
		(make-op :action 'take-aste-3080
			:add-list '(aste-3080))
		(make-op :action 'take-aste-3090
			:preconds '(jcom-3090)
			:add-list '(aste-3090))
		(make-op :action 'take-aste-3240
			:preconds '(aste-2710)
			:add-list '(aste-3240))
		(make-op :action 'take-aste-3300
			:add-list '(aste-3300))
		(make-op :action 'take-aste-3440
			:preconds '(tee-3440)
			:add-list '(aste-3440))
		(make-op :action 'take-aste-3500
			:add-list '(aste-3500))
		(make-op :action 'take-aste-3600
			:preconds '(math-1050 stat-1040 stat-1045)
			:add-list '(aste-3600))
		(make-op :action 'take-aste-3620
			:add-list '(aste-3620))
		(make-op :action 'take-aste-3625
			:add-list '(aste-3625))
		(make-op :action 'take-aste-3670
			:preconds '(math-1050)
			:add-list '(aste-3670))
		(make-op :action 'take-aste-3710
			:add-list '(aste-3710))
		(make-op :action 'take-aste-3720
			:preconds '(aste-1620)
			:add-list '(aste-3720))
		(make-op :action 'take-aste-3730
			:preconds '(aste-3720)
			:add-list '(aste-3730))
		(make-op :action 'take-aste-3900
			:add-list '(aste-3900))
		(make-op :action 'take-aste-4100
			:preconds '(math-1050)
			:add-list '(aste-4100))
		(make-op :action 'take-aste-4150
			:preconds '(aste-2710 aste-3240)
			:add-list '(aste-4150))
		(make-op :action 'take-aste-4155
			:add-list '(aste-4155))
		(make-op :action 'take-aste-4210
			:preconds '(bsed-4210 fcse-4210 tee-4210)
			:add-list '(aste-4210))
		(make-op :action 'take-aste-4215
			:add-list '(aste-4215))
		(make-op :action 'take-aste-4250
			:add-list '(aste-4250))
		(make-op :action 'take-aste-4300
			:add-list '(aste-4300))
		(make-op :action 'take-aste-4400
			:add-list '(aste-4400))
		(make-op :action 'take-aste-4900
			:add-list '(aste-4900))
		(make-op :action 'take-aste-5150
			:add-list '(aste-5150))
		(make-op :action 'take-aste-5151
			:add-list '(aste-5151))
		(make-op :action 'take-aste-5152
			:add-list '(aste-5152))
		(make-op :action 'take-aste-5153
			:add-list '(aste-5153))
		(make-op :action 'take-aste-5154
			:add-list '(aste-5154))
		(make-op :action 'take-aste-5220
			:preconds '(aste-6220)
			:add-list '(aste-5220))
		(make-op :action 'take-aste-5260
			:add-list '(aste-5260))
		(make-op :action 'take-aste-5400
			:preconds '(aste-6400)
			:add-list '(aste-5400))
		(make-op :action 'take-aste-5450
			:preconds '(ndfs-5450)
			:add-list '(aste-5450))
		(make-op :action 'take-aste-5500
			:add-list '(aste-5500))
		(make-op :action 'take-aste-5550
			:add-list '(aste-5550))
		(make-op :action 'take-aste-5600
			:preconds '(cai-5600 aste-6600)
			:add-list '(aste-5600))
		(make-op :action 'take-aste-5610
			:preconds '(aste-5600 aste-6600 cai-5600 cai-6600 cai-5610 aste-6610)
			:add-list '(aste-5610))
		(make-op :action 'take-aste-5620
			:preconds '(cai-5620 aste-6620)
			:add-list '(aste-5620))
		(make-op :action 'take-aste-5630
			:add-list '(aste-5630))
		(make-op :action 'take-aste-5635
			:preconds '(aste-3240 aste-4150)
			:add-list '(aste-5635))
		(make-op :action 'take-aste-5640
			:add-list '(aste-5640))
		(make-op :action 'take-advs-1050
			:add-list '(advs-1050))
		(make-op :action 'take-advs-1100
			:add-list '(advs-1100))
		(make-op :action 'take-advs-1110
			:add-list '(advs-1110))
		(make-op :action 'take-advs-1200
			:add-list '(advs-1200))
		(make-op :action 'take-advs-1600
			:preconds '(advs-1200)
			:add-list '(advs-1600))
		(make-op :action 'take-advs-2010
			:add-list '(advs-2010))
		(make-op :action 'take-advs-2020
			:add-list '(advs-2020))
		(make-op :action 'take-advs-2030
			:add-list '(advs-2030))
		(make-op :action 'take-advs-2040
			:preconds '(advs-2020)
			:add-list '(advs-2040))
		(make-op :action 'take-advs-2050
			:preconds '(advs-2010 advs-2020 advs-2030 advs-2040)
			:add-list '(advs-2050))
		(make-op :action 'take-advs-2060
			:preconds '(advs-2010)
			:add-list '(advs-2060))
		(make-op :action 'take-advs-2080
			:add-list '(advs-2080))
		(make-op :action 'take-advs-2090
			:preconds '(advs-1110)
			:add-list '(advs-2090))
		(make-op :action 'take-advs-2100
			:add-list '(advs-2100))
		(make-op :action 'take-advs-2120
			:preconds '(advs-1110)
			:add-list '(advs-2120))
		(make-op :action 'take-advs-2150
			:add-list '(advs-2150))
		(make-op :action 'take-advs-2160
			:preconds '(advs-1110)
			:add-list '(advs-2160))
		(make-op :action 'take-advs-2190
			:add-list '(advs-2190))
		(make-op :action 'take-advs-2200
			:preconds '(biol-1620)
			:add-list '(advs-2200))
		(make-op :action 'take-advs-2250
			:add-list '(advs-2250))
		(make-op :action 'take-advs-2300
			:add-list '(advs-2300))
		(make-op :action 'take-advs-2350
			:add-list '(advs-2350))
		(make-op :action 'take-advs-2400
			:preconds '(advs-1600)
			:add-list '(advs-2400))
		(make-op :action 'take-advs-2500
			:add-list '(advs-2500))
		(make-op :action 'take-advs-2600
			:preconds '(advs-1600)
			:add-list '(advs-2600))
		(make-op :action 'take-advs-2650
			:preconds '(advs-1600)
			:add-list '(advs-2650))
		(make-op :action 'take-advs-2800
			:preconds '(advs-1200)
			:add-list '(advs-2800))
		(make-op :action 'take-advs-2810
			:add-list '(advs-2810))
		(make-op :action 'take-advs-3000
			:preconds '(advs-2200)
			:add-list '(advs-3000))
		(make-op :action 'take-advs-3100
			:add-list '(advs-3100))
		(make-op :action 'take-advs-3150
			:add-list '(advs-3150))
		(make-op :action 'take-advs-3170
			:preconds '(advs-3150)
			:add-list '(advs-3170))
		(make-op :action 'take-advs-3200
			:add-list '(advs-3200))
		(make-op :action 'take-advs-3300
			:add-list '(advs-3300))
		(make-op :action 'take-advs-3400
			:preconds '(advs-3150)
			:add-list '(advs-3400))
		(make-op :action 'take-advs-3500
			:preconds '(advs-2200)
			:add-list '(advs-3500))
		(make-op :action 'take-advs-3520
			:preconds '(advs-2200)
			:add-list '(advs-3520))
		(make-op :action 'take-advs-3600
			:preconds '(advs-2600 advs-2650)
			:add-list '(advs-3600))
		(make-op :action 'take-advs-3610
			:preconds '(advs-3150)
			:add-list '(advs-3610))
		(make-op :action 'take-advs-3650
			:add-list '(advs-3650))
		(make-op :action 'take-advs-3690
			:preconds '(advs-3150)
			:add-list '(advs-3690))
		(make-op :action 'take-advs-3710
			:preconds '(advs-3650)
			:add-list '(advs-3710))
		(make-op :action 'take-advs-3750
			:preconds '(advs-3600)
			:add-list '(advs-3750))
		(make-op :action 'take-advs-3900
			:add-list '(advs-3900))
		(make-op :action 'take-advs-3910
			:add-list '(advs-3910))
		(make-op :action 'take-advs-3930
			:add-list '(advs-3930))
		(make-op :action 'take-advs-4200
			:preconds '(advs-2200 chem-1120 chem-1220 chem-2310)
			:add-list '(advs-4200))
		(make-op :action 'take-advs-4210
			:preconds '(advs-4200)
			:add-list '(advs-4210))
		(make-op :action 'take-advs-4220
			:preconds '(advs-4200)
			:add-list '(advs-4220))
		(make-op :action 'take-advs-4230
			:preconds '(advs-3170 advs-3400 advs-3610)
			:add-list '(advs-4230))
		(make-op :action 'take-advs-4250
			:add-list '(advs-4250))
		(make-op :action 'take-advs-4260
			:preconds '(advs-5160 advs-5260)
			:add-list '(advs-4260))
		(make-op :action 'take-advs-4270
			:add-list '(advs-4270))
		(make-op :action 'take-advs-4560
			:preconds '(biol-1620)
			:add-list '(advs-4560))
		(make-op :action 'take-advs-4800
			:add-list '(advs-4800))
		(make-op :action 'take-advs-4810
			:add-list '(advs-4810))
		(make-op :action 'take-advs-4900
			:add-list '(advs-4900))
		(make-op :action 'take-advs-4920
			:add-list '(advs-4920))
		(make-op :action 'take-advs-5000
			:preconds '(biol-1610 chem-2310 chem-3700 biol-3300)
			:add-list '(advs-5000))
		(make-op :action 'take-advs-5030
			:preconds '(advs-1110)
			:add-list '(advs-5030))
		(make-op :action 'take-advs-5080
			:preconds '(advs-2080 advs-4200 advs-4560 advs-6080)
			:add-list '(advs-5080))
		(make-op :action 'take-advs-5090
			:preconds '(advs-2090 advs-4200 advs-4560 advs-6090)
			:add-list '(advs-5090))
		(make-op :action 'take-advs-5110
			:preconds '(biol-1610 biol-1620 chem-1210 chem-1220 biol-5110 advs-6110)
			:add-list '(advs-5110))
		(make-op :action 'take-advs-5120
			:preconds '(advs-2120 advs-4200 advs-4560 advs-6120)
			:add-list '(advs-5120))
		(make-op :action 'take-advs-5130
			:preconds '(advs-4200 advs-4560 advs-6130)
			:add-list '(advs-5130))
		(make-op :action 'take-advs-5160
			:preconds '(biol-5160 ndfs-5160 psc-5160)
			:add-list '(advs-5160))
		(make-op :action 'take-advs-5190
			:preconds '(advs-2190 advs-4200 advs-4560 advs-6190)
			:add-list '(advs-5190))
		(make-op :action 'take-advs-5220
			:preconds '(chem-3700 biol-5220 ndfs-5220 advs-6220)
			:add-list '(advs-5220))
		(make-op :action 'take-advs-5260
			:preconds '(chem-3700 chem-5710 biol-3060 biol-5260 ndfs-5260 psc-5260)
			:add-list '(advs-5260))
		(make-op :action 'take-advs-5280
			:preconds '(advs-5260 biol-5280 advs-6280)
			:add-list '(advs-5280))
		(make-op :action 'take-advs-5300
			:preconds '(advs-6300)
			:add-list '(advs-5300))
		(make-op :action 'take-advs-5350
			:preconds '(biol-5600 chem-3700 advs-6350)
			:add-list '(advs-5350))
		(make-op :action 'take-advs-5400
			:preconds '(chem-1220 biol-1620 chem-2300 biol-5400 pubh-5400 advs-6400)
			:add-list '(advs-5400))
		(make-op :action 'take-advs-5500
			:preconds '(advs-3500 advs-6500)
			:add-list '(advs-5500))
		(make-op :action 'take-advs-5520
			:preconds '(wild-4000 advs-6520)
			:add-list '(advs-5520))
		(make-op :action 'take-advs-5530
			:preconds '(advs-3500 advs-2200 advs-6530)
			:add-list '(advs-5530))
		(make-op :action 'take-advs-5630
			:add-list '(advs-5630))
		(make-op :action 'take-advs-5650
			:add-list '(advs-5650))
		(make-op :action 'take-advs-5820
			:preconds '(advs-4560 biol-3060 advs-6820)
			:add-list '(advs-5820))
		(make-op :action 'take-advs-5860
			:preconds '(wild-5860)
			:add-list '(advs-5860))
		(make-op :action 'take-advs-5900
			:add-list '(advs-5900))
		(make-op :action 'take-advs-5910
			:add-list '(advs-5910))
		(make-op :action 'take-advs-5920
			:add-list '(advs-5920))
		(make-op :action 'take-anth-1010
			:add-list '(anth-1010))
		(make-op :action 'take-anth-1020
			:add-list '(anth-1020))
		(make-op :action 'take-anth-1030
			:add-list '(anth-1030))
		(make-op :action 'take-anth-1090
			:preconds '(rels-1090)
			:add-list '(anth-1090))
		(make-op :action 'take-anth-1099
			:add-list '(anth-1099))
		(make-op :action 'take-anth-2010
			:add-list '(anth-2010))
		(make-op :action 'take-anth-2011
			:add-list '(anth-2011))
		(make-op :action 'take-anth-2018
			:add-list '(anth-2018))
		(make-op :action 'take-anth-2210
			:preconds '(engl-2210 hist-2210)
			:add-list '(anth-2210))
		(make-op :action 'take-anth-2330
			:add-list '(anth-2330))
		(make-op :action 'take-anth-2520
			:add-list '(anth-2520))
		(make-op :action 'take-anth-2525
			:add-list '(anth-2525))
		(make-op :action 'take-anth-2720
			:preconds '(engl-2720 hist-2720)
			:add-list '(anth-2720))
		(make-op :action 'take-anth-2977
			:add-list '(anth-2977))
		(make-op :action 'take-anth-2988
			:add-list '(anth-2988))
		(make-op :action 'take-anth-2999
			:add-list '(anth-2999))
		(make-op :action 'take-anth-3090
			:add-list '(anth-3090))
		(make-op :action 'take-anth-3110
			:add-list '(anth-3110))
		(make-op :action 'take-anth-3120
			:add-list '(anth-3120))
		(make-op :action 'take-anth-3130
			:add-list '(anth-3130))
		(make-op :action 'take-anth-3140
			:add-list '(anth-3140))
		(make-op :action 'take-anth-3150
			:add-list '(anth-3150))
		(make-op :action 'take-anth-3160
			:preconds '(rels-3160)
			:add-list '(anth-3160))
		(make-op :action 'take-anth-3165
			:preconds '(rels-3165)
			:add-list '(anth-3165))
		(make-op :action 'take-anth-3180
			:add-list '(anth-3180))
		(make-op :action 'take-anth-3200
			:add-list '(anth-3200))
		(make-op :action 'take-anth-3225
			:add-list '(anth-3225))
		(make-op :action 'take-anth-3240
			:add-list '(anth-3240))
		(make-op :action 'take-anth-3250
			:add-list '(anth-3250))
		(make-op :action 'take-anth-3300
			:add-list '(anth-3300))
		(make-op :action 'take-anth-3310
			:add-list '(anth-3310))
		(make-op :action 'take-anth-3320
			:preconds '(envs-3320)
			:add-list '(anth-3320))
		(make-op :action 'take-anth-3330
			:add-list '(anth-3330))
		(make-op :action 'take-anth-3340
			:add-list '(anth-3340))
		(make-op :action 'take-anth-3350
			:add-list '(anth-3350))
		(make-op :action 'take-anth-3360
			:add-list '(anth-3360))
		(make-op :action 'take-anth-3370
			:add-list '(anth-3370))
		(make-op :action 'take-anth-3380
			:add-list '(anth-3380))
		(make-op :action 'take-anth-3390
			:preconds '(hist-3390)
			:add-list '(anth-3390))
		(make-op :action 'take-anth-3550
			:add-list '(anth-3550))
		(make-op :action 'take-anth-3710
			:preconds '(engl-3710 hist-3710 rels-3710)
			:add-list '(anth-3710))
		(make-op :action 'take-anth-4100
			:preconds '(ling-4100)
			:add-list '(anth-4100))
		(make-op :action 'take-anth-4110
			:preconds '(anth-6110)
			:add-list '(anth-4110))
		(make-op :action 'take-anth-4115
			:add-list '(anth-4115))
		(make-op :action 'take-anth-4120
			:add-list '(anth-4120))
		(make-op :action 'take-anth-4130
			:add-list '(anth-4130))
		(make-op :action 'take-anth-4140
			:preconds '(anth-6140)
			:add-list '(anth-4140))
		(make-op :action 'take-anth-4150
			:add-list '(anth-4150))
		(make-op :action 'take-anth-4160
			:add-list '(anth-4160))
		(make-op :action 'take-anth-4170
			:preconds '(anth-6170)
			:add-list '(anth-4170))
		(make-op :action 'take-anth-4180
			:add-list '(anth-4180))
		(make-op :action 'take-anth-4190
			:add-list '(anth-4190))
		(make-op :action 'take-anth-4200
			:add-list '(anth-4200))
		(make-op :action 'take-anth-4210
			:preconds '(envs-4210 anth-6210)
			:add-list '(anth-4210))
		(make-op :action 'take-anth-4240
			:add-list '(anth-4240))
		(make-op :action 'take-anth-4250
			:add-list '(anth-4250))
		(make-op :action 'take-anth-4330
			:add-list '(anth-4330))
		(make-op :action 'take-anth-4650
			:preconds '(engl-4650)
			:add-list '(anth-4650))
		(make-op :action 'take-anth-4800
			:add-list '(anth-4800))
		(make-op :action 'take-anth-4980
			:preconds '(anth-1010)
			:add-list '(anth-4980))
		(make-op :action 'take-anth-4990
			:preconds '(anth-1010 anth-4980)
			:add-list '(anth-4990))
		(make-op :action 'take-anth-5115
			:preconds '(anth-6115)
			:add-list '(anth-5115))
		(make-op :action 'take-anth-5130
			:preconds '(soc-5130 anth-6130)
			:add-list '(anth-5130))
		(make-op :action 'take-anth-5190
			:preconds '(anth-6190)
			:add-list '(anth-5190))
		(make-op :action 'take-anth-5195
			:preconds '(anth-1090 rels-1090 anth-3160 rels-1010 rels-5195)
			:add-list '(anth-5195))
		(make-op :action 'take-anth-5210
			:add-list '(anth-5210))
		(make-op :action 'take-anth-5250
			:preconds '(stat-1040 stat-1045 anth-3250 anth-6250)
			:add-list '(anth-5250))
		(make-op :action 'take-anth-5300
			:preconds '(anth-1030 anth-6300)
			:add-list '(anth-5300))
		(make-op :action 'take-anth-5310
			:preconds '(anth-6310)
			:add-list '(anth-5310))
		(make-op :action 'take-anth-5320
			:preconds '(anth-6320)
			:add-list '(anth-5320))
		(make-op :action 'take-anth-5330
			:preconds '(anth-1020 anth-1030 geo-1110 geo-2200 anth-6330)
			:add-list '(anth-5330))
		(make-op :action 'take-anth-5340
			:preconds '(anth-1030 anth-2330)
			:add-list '(anth-5340))
		(make-op :action 'take-anth-5380
			:preconds '(anth-1030 anth-2330 anth-6380)
			:add-list '(anth-5380))
		(make-op :action 'take-anth-5420
			:preconds '(anth-1030 anth-6420)
			:add-list '(anth-5420))
		(make-op :action 'take-anth-5650
			:preconds '(anth-6650)
			:add-list '(anth-5650))
		(make-op :action 'take-anth-5700
			:preconds '(engl-5700 hist-5700)
			:add-list '(anth-5700))
		(make-op :action 'take-anth-5800
			:preconds '(anth-6800)
			:add-list '(anth-5800))
		(make-op :action 'take-anth-5900
			:add-list '(anth-5900))
		(make-op :action 'take-anth-5980
			:add-list '(anth-5980))
		(make-op :action 'take-cai-4300
			:preconds '(cai-5000 cai-6300)
			:add-list '(cai-4300))
		(make-op :action 'take-cai-5000
			:add-list '(cai-5000))
		(make-op :action 'take-cai-5010
			:add-list '(cai-5010))
		(make-op :action 'take-cai-5020
			:add-list '(cai-5020))
		(make-op :action 'take-cai-5200
			:preconds '(cai-5000)
			:add-list '(cai-5200))
		(make-op :action 'take-cai-5310
			:preconds '(cai-5000 cai-6310)
			:add-list '(cai-5310))
		(make-op :action 'take-cai-5400
			:preconds '(cai-5000 phil-5400 cai-6400)
			:add-list '(cai-5400))
		(make-op :action 'take-cai-5500
			:preconds '(cai-6500)
			:add-list '(cai-5500))
		(make-op :action 'take-cai-5510
			:preconds '(cai-6510)
			:add-list '(cai-5510))
		(make-op :action 'take-cai-5520
			:preconds '(cai-6520)
			:add-list '(cai-5520))
		(make-op :action 'take-cai-5530
			:preconds '(cai-6530)
			:add-list '(cai-5530))
		(make-op :action 'take-cai-5540
			:preconds '(cai-6540)
			:add-list '(cai-5540))
		(make-op :action 'take-cai-5550
			:preconds '(cai-6550)
			:add-list '(cai-5550))
		(make-op :action 'take-cai-5600
			:preconds '(aste-5600 cai-6600)
			:add-list '(cai-5600))
		(make-op :action 'take-cai-5610
			:preconds '(cai-5600 aste-5610 cai-6610)
			:add-list '(cai-5610))
		(make-op :action 'take-cai-5620
			:preconds '(aste-5620 cai-6620)
			:add-list '(cai-5620))
		(make-op :action 'take-cai-5630
			:preconds '(cai-6630)
			:add-list '(cai-5630))
		(make-op :action 'take-cai-5800
			:add-list '(cai-5800))
		(make-op :action 'take-cai-5810
			:add-list '(cai-5810))
		(make-op :action 'take-cai-5820
			:add-list '(cai-5820))
		(make-op :action 'take-cai-5830
			:add-list '(cai-5830))
		(make-op :action 'take-cai-5880
			:add-list '(cai-5880))
		(make-op :action 'take-cai-5890
			:preconds '(cai-6890)
			:add-list '(cai-5890))
		(make-op :action 'take-cai-5900
			:preconds '(cai-6900)
			:add-list '(cai-5900))
		(make-op :action 'take-cai-5910
			:preconds '(cai-6910)
			:add-list '(cai-5910))
		(make-op :action 'take-cai-5920
			:preconds '(cai-6920)
			:add-list '(cai-5920))
		(make-op :action 'take-cai-5930
			:preconds '(cai-6930)
			:add-list '(cai-5930))
		(make-op :action 'take-cai-5940
			:preconds '(cai-6940)
			:add-list '(cai-5940))
		(make-op :action 'take-apec-1400
			:add-list '(apec-1400))
		(make-op :action 'take-apec-1600
			:add-list '(apec-1600))
		(make-op :action 'take-apec-2010
			:preconds '(apec-1600 ecn-1500)
			:add-list '(apec-2010))
		(make-op :action 'take-apec-2120
			:add-list '(apec-2120))
		(make-op :action 'take-apec-2310
			:preconds '(apec-2120)
			:add-list '(apec-2310))
		(make-op :action 'take-apec-2500
			:add-list '(apec-2500))
		(make-op :action 'take-apec-2830
			:add-list '(apec-2830))
		(make-op :action 'take-apec-2900
			:add-list '(apec-2900))
		(make-op :action 'take-apec-3010
			:add-list '(apec-3010))
		(make-op :action 'take-apec-3012
			:add-list '(apec-3012))
		(make-op :action 'take-apec-3310
			:preconds '(apec-2010)
			:add-list '(apec-3310))
		(make-op :action 'take-apec-3400
			:add-list '(apec-3400))
		(make-op :action 'take-apec-4010
			:preconds '(ecn-3010 ecn-4010 apec-2010 ecn-2010 math-1100 math-1210 stat-2300 stat-2000)
			:add-list '(apec-4010))
		(make-op :action 'take-apec-4250
			:add-list '(apec-4250))
		(make-op :action 'take-apec-4310
			:add-list '(apec-4310))
		(make-op :action 'take-apec-4900
			:add-list '(apec-4900))
		(make-op :action 'take-apec-5000
			:preconds '(apec-3010 apec-3310 apec-4010 ecn-4010)
			:add-list '(apec-5000))
		(make-op :action 'take-apec-5010
			:preconds '(apec-3010 apec-3310)
			:add-list '(apec-5010))
		(make-op :action 'take-apec-5015
			:preconds '(apec-2310 apec-3310)
			:add-list '(apec-5015))
		(make-op :action 'take-apec-5020
			:preconds '(apec-3010 apec-3310)
			:add-list '(apec-5020))
		(make-op :action 'take-apec-5040
			:add-list '(apec-5040))
		(make-op :action 'take-apec-5150
			:preconds '(acct-2010 apec-3010)
			:add-list '(apec-5150))
		(make-op :action 'take-apec-5300
			:preconds '(apec-3310 apec-6300)
			:add-list '(apec-5300))
		(make-op :action 'take-apec-5330
			:preconds '(stat-2000 stat-2300 stat-3000)
			:add-list '(apec-5330))
		(make-op :action 'take-apec-5500
			:preconds '(math-1100 math-1210)
			:add-list '(apec-5500))
		(make-op :action 'take-apec-5560
			:preconds '(apec-2010 ecn-2010 apec-3012)
			:add-list '(apec-5560))
		(make-op :action 'take-apec-5700
			:preconds '(apec-6700)
			:add-list '(apec-5700))
		(make-op :action 'take-apec-5710
			:preconds '(apec-6710)
			:add-list '(apec-5710))
		(make-op :action 'take-apec-5940
			:add-list '(apec-5940))
		(make-op :action 'take-apec-5950
			:add-list '(apec-5950))
		(make-op :action 'take-appr-2301
			:add-list '(appr-2301))
		(make-op :action 'take-appr-2302
			:add-list '(appr-2302))
		(make-op :action 'take-appr-2303
			:add-list '(appr-2303))
		(make-op :action 'take-appr-2304
			:add-list '(appr-2304))
		(make-op :action 'take-appr-2310
			:add-list '(appr-2310))
		(make-op :action 'take-appr-2320
			:add-list '(appr-2320))
		(make-op :action 'take-appr-2410
			:add-list '(appr-2410))
		(make-op :action 'take-appr-2420
			:add-list '(appr-2420))
		(make-op :action 'take-appr-2988
			:add-list '(appr-2988))
		(make-op :action 'take-arbc-1010
			:add-list '(arbc-1010))
		(make-op :action 'take-arbc-1020
			:preconds '(arbc-1010)
			:add-list '(arbc-1020))
		(make-op :action 'take-arbc-1800
			:add-list '(arbc-1800))
		(make-op :action 'take-arbc-2010
			:add-list '(arbc-2010))
		(make-op :action 'take-arbc-2020
			:add-list '(arbc-2020))
		(make-op :action 'take-arbc-2800
			:add-list '(arbc-2800))
		(make-op :action 'take-arbc-3010
			:preconds '(arbc-2020)
			:add-list '(arbc-3010))
		(make-op :action 'take-arbc-3020
			:preconds '(arbc-3010)
			:add-list '(arbc-3020))
		(make-op :action 'take-arbc-3030
			:add-list '(arbc-3030))
		(make-op :action 'take-arbc-3800
			:add-list '(arbc-3800))
		(make-op :action 'take-arbc-4040
			:add-list '(arbc-4040))
		(make-op :action 'take-art-1010
			:add-list '(art-1010))
		(make-op :action 'take-art-1020
			:add-list '(art-1020))
		(make-op :action 'take-art-1050
			:add-list '(art-1050))
		(make-op :action 'take-art-1120
			:add-list '(art-1120))
		(make-op :action 'take-art-1130
			:add-list '(art-1130))
		(make-op :action 'take-art-1140
			:add-list '(art-1140))
		(make-op :action 'take-art-1430
			:add-list '(art-1430))
		(make-op :action 'take-art-2110
			:preconds '(art-1020 art-1020)
			:add-list '(art-2110))
		(make-op :action 'take-art-2200
			:preconds '(art-1020 art-1120)
			:add-list '(art-2200))
		(make-op :action 'take-art-2220
			:preconds '(art-2200)
			:add-list '(art-2220))
		(make-op :action 'take-art-2230
			:preconds '(art-1020 art-1120)
			:add-list '(art-2230))
		(make-op :action 'take-art-2260
			:add-list '(art-2260))
		(make-op :action 'take-art-2400
			:preconds '(art-1020 art-1120 art-1130)
			:add-list '(art-2400))
		(make-op :action 'take-art-2500
			:add-list '(art-2500))
		(make-op :action 'take-art-2600
			:preconds '(art-1130)
			:add-list '(art-2600))
		(make-op :action 'take-art-2650
			:add-list '(art-2650))
		(make-op :action 'take-art-2660
			:add-list '(art-2660))
		(make-op :action 'take-art-2680
			:preconds '(art-2110)
			:add-list '(art-2680))
		(make-op :action 'take-art-2690
			:add-list '(art-2690))
		(make-op :action 'take-art-2700
			:add-list '(art-2700))
		(make-op :action 'take-art-2810
			:add-list '(art-2810))
		(make-op :action 'take-art-2830
			:preconds '(art-2810)
			:add-list '(art-2830))
		(make-op :action 'take-art-2900
			:add-list '(art-2900))
		(make-op :action 'take-art-2920
			:preconds '(art-1020 art-1120 art-1130)
			:add-list '(art-2920))
		(make-op :action 'take-art-3000
			:add-list '(art-3000))
		(make-op :action 'take-art-3050
			:preconds '(japn-3050)
			:add-list '(art-3050))
		(make-op :action 'take-art-3200
			:preconds '(art-2200 art-2200)
			:add-list '(art-3200))
		(make-op :action 'take-art-3220
			:preconds '(art-2230)
			:add-list '(art-3220))
		(make-op :action 'take-art-3230
			:preconds '(art-2230)
			:add-list '(art-3230))
		(make-op :action 'take-art-3240
			:preconds '(art-2230)
			:add-list '(art-3240))
		(make-op :action 'take-art-3250
			:preconds '(art-2230)
			:add-list '(art-3250))
		(make-op :action 'take-art-3260
			:preconds '(art-1020 art-1120 art-1130 art-2230)
			:add-list '(art-3260))
		(make-op :action 'take-art-3270
			:add-list '(art-3270))
		(make-op :action 'take-art-3280
			:preconds '(art-2230)
			:add-list '(art-3280))
		(make-op :action 'take-art-3300
			:add-list '(art-3300))
		(make-op :action 'take-art-3370
			:add-list '(art-3370))
		(make-op :action 'take-art-3400
			:add-list '(art-3400))
		(make-op :action 'take-art-3410
			:preconds '(art-2400)
			:add-list '(art-3410))
		(make-op :action 'take-art-3420
			:add-list '(art-3420))
		(make-op :action 'take-art-3610
			:add-list '(art-3610))
		(make-op :action 'take-art-3650
			:preconds '(art-2650)
			:add-list '(art-3650))
		(make-op :action 'take-art-3660
			:preconds '(art-2650)
			:add-list '(art-3660))
		(make-op :action 'take-art-3700
			:preconds '(eled-3000 eled-3001 art-2200 art-2230 art-2400 art-2650 art-2810)
			:add-list '(art-3700))
		(make-op :action 'take-art-3810
			:preconds '(art-2810)
			:add-list '(art-3810))
		(make-op :action 'take-art-4000
			:preconds '(art-3000)
			:add-list '(art-4000))
		(make-op :action 'take-art-4200
			:preconds '(art-3200)
			:add-list '(art-4200))
		(make-op :action 'take-art-4210
			:preconds '(art-2200)
			:add-list '(art-4210))
		(make-op :action 'take-art-4220
			:preconds '(art-2200)
			:add-list '(art-4220))
		(make-op :action 'take-art-4250
			:preconds '(art-3220 art-3230 art-3240 art-3250)
			:add-list '(art-4250))
		(make-op :action 'take-art-4260
			:preconds '(art-2110)
			:add-list '(art-4260))
		(make-op :action 'take-art-4270
			:preconds '(art-2200 art-3200)
			:add-list '(art-4270))
		(make-op :action 'take-art-4300
			:preconds '(art-3300)
			:add-list '(art-4300))
		(make-op :action 'take-art-4410
			:preconds '(art-3400)
			:add-list '(art-4410))
		(make-op :action 'take-art-4420
			:preconds '(art-3400)
			:add-list '(art-4420))
		(make-op :action 'take-art-4425
			:preconds '(art-4420)
			:add-list '(art-4425))
		(make-op :action 'take-art-4430
			:preconds '(art-4410)
			:add-list '(art-4430))
		(make-op :action 'take-art-4440
			:preconds '(art-3400)
			:add-list '(art-4440))
		(make-op :action 'take-art-4445
			:preconds '(art-4440)
			:add-list '(art-4445))
		(make-op :action 'take-art-4450
			:preconds '(art-4410 art-4420 art-4440)
			:add-list '(art-4450))
		(make-op :action 'take-art-4455
			:preconds '(art-4440)
			:add-list '(art-4455))
		(make-op :action 'take-art-4460
			:preconds '(art-4440)
			:add-list '(art-4460))
		(make-op :action 'take-art-4470
			:add-list '(art-4470))
		(make-op :action 'take-art-4475
			:add-list '(art-4475))
		(make-op :action 'take-art-4480
			:preconds '(art-6480)
			:add-list '(art-4480))
		(make-op :action 'take-art-4610
			:preconds '(art-3610)
			:add-list '(art-4610))
		(make-op :action 'take-art-4620
			:preconds '(art-4660)
			:add-list '(art-4620))
		(make-op :action 'take-art-4630
			:preconds '(art-2600 art-6630)
			:add-list '(art-4630))
		(make-op :action 'take-art-4640
			:preconds '(art-3650 art-3660)
			:add-list '(art-4640))
		(make-op :action 'take-art-4650
			:preconds '(art-3650 art-3660)
			:add-list '(art-4650))
		(make-op :action 'take-art-4660
			:preconds '(art-4610)
			:add-list '(art-4660))
		(make-op :action 'take-art-4810
			:preconds '(art-3810)
			:add-list '(art-4810))
		(make-op :action 'take-art-4820
			:preconds '(art-3810)
			:add-list '(art-4820))
		(make-op :action 'take-art-4825
			:preconds '(art-3810)
			:add-list '(art-4825))
		(make-op :action 'take-art-4835
			:preconds '(art-3810)
			:add-list '(art-4835))
		(make-op :action 'take-art-4865
			:preconds '(art-3810)
			:add-list '(art-4865))
		(make-op :action 'take-art-4875
			:preconds '(art-3810)
			:add-list '(art-4875))
		(make-op :action 'take-art-4885
			:preconds '(art-3810)
			:add-list '(art-4885))
		(make-op :action 'take-art-4900
			:add-list '(art-4900))
		(make-op :action 'take-art-4910
			:add-list '(art-4910))
		(make-op :action 'take-art-4915
			:preconds '(art-4910)
			:add-list '(art-4915))
		(make-op :action 'take-art-4920
			:add-list '(art-4920))
		(make-op :action 'take-art-5500
			:add-list '(art-5500))
		(make-op :action 'take-art-5630
			:add-list '(art-5630))
		(make-op :action 'take-cca-1150
			:add-list '(cca-1150))
		(make-op :action 'take-cca-1250
			:add-list '(cca-1250))
		(make-op :action 'take-cca-2250
			:add-list '(cca-2250))
		(make-op :action 'take-cca-3050
			:add-list '(cca-3050))
		(make-op :action 'take-cca-3060
			:add-list '(cca-3060))
		(make-op :action 'take-cca-3070
			:add-list '(cca-3070))
		(make-op :action 'take-cca-3330
			:add-list '(cca-3330))
		(make-op :action 'take-cca-4250
			:add-list '(cca-4250))
		(make-op :action 'take-cca-4910
			:add-list '(cca-4910))
		(make-op :action 'take-cca-5250
			:add-list '(cca-5250))
		(make-op :action 'take-arth-1270
			:add-list '(arth-1270))
		(make-op :action 'take-arth-2710
			:add-list '(arth-2710))
		(make-op :action 'take-arth-2720
			:add-list '(arth-2720))
		(make-op :action 'take-arth-2730
			:add-list '(arth-2730))
		(make-op :action 'take-arth-3110
			:preconds '(hist-3110)
			:add-list '(arth-3110))
		(make-op :action 'take-arth-3200
			:add-list '(arth-3200))
		(make-op :action 'take-arth-3210
			:preconds '(clas-3210 engl-3210 hist-3210 rels-3210)
			:add-list '(arth-3210))
		(make-op :action 'take-arth-3215
			:preconds '(arth-2710)
			:add-list '(arth-3215))
		(make-op :action 'take-arth-3220
			:preconds '(arth-2710)
			:add-list '(arth-3220))
		(make-op :action 'take-arth-3270
			:preconds '(arth-2710 arth-2720)
			:add-list '(arth-3270))
		(make-op :action 'take-arth-3295
			:preconds '(arth-2710 arth-2720)
			:add-list '(arth-3295))
		(make-op :action 'take-arth-3310
			:preconds '(arth-2710 arth-2720)
			:add-list '(arth-3310))
		(make-op :action 'take-arth-3320
			:preconds '(arth-2710 arth-2720)
			:add-list '(arth-3320))
		(make-op :action 'take-arth-3340
			:preconds '(arth-2710 arth-2720)
			:add-list '(arth-3340))
		(make-op :action 'take-arth-3420
			:preconds '(arth-2710 arth-2720)
			:add-list '(arth-3420))
		(make-op :action 'take-arth-3510
			:preconds '(arth-2710 rels-3510)
			:add-list '(arth-3510))
		(make-op :action 'take-arth-3610
			:preconds '(arth-2710)
			:add-list '(arth-3610))
		(make-op :action 'take-arth-3615
			:preconds '(arth-2710)
			:add-list '(arth-3615))
		(make-op :action 'take-arth-3620
			:preconds '(arth-2710)
			:add-list '(arth-3620))
		(make-op :action 'take-arth-3630
			:preconds '(arth-2710)
			:add-list '(arth-3630))
		(make-op :action 'take-arth-3710
			:add-list '(arth-3710))
		(make-op :action 'take-arth-3720
			:preconds '(arth-2720)
			:add-list '(arth-3720))
		(make-op :action 'take-arth-3740
			:preconds '(arth-2720)
			:add-list '(arth-3740))
		(make-op :action 'take-arth-3750
			:preconds '(arth-2720)
			:add-list '(arth-3750))
		(make-op :action 'take-arth-3755
			:preconds '(arth-2720)
			:add-list '(arth-3755))
		(make-op :action 'take-arth-3760
			:preconds '(arth-2720)
			:add-list '(arth-3760))
		(make-op :action 'take-arth-3780
			:preconds '(arth-2720)
			:add-list '(arth-3780))
		(make-op :action 'take-arth-3820
			:add-list '(arth-3820))
		(make-op :action 'take-arth-3840
			:add-list '(arth-3840))
		(make-op :action 'take-arth-3860
			:preconds '(arth-2710 arth-2720)
			:add-list '(arth-3860))
		(make-op :action 'take-arth-4210
			:add-list '(arth-4210))
		(make-op :action 'take-arth-4260
			:preconds '(arth-2710 arth-2720)
			:add-list '(arth-4260))
		(make-op :action 'take-arth-4310
			:preconds '(arth-2710 arth-2720 arth-6310)
			:add-list '(arth-4310))
		(make-op :action 'take-arth-4410
			:preconds '(arth-2710 arth-2720 arth-6410)
			:add-list '(arth-4410))
		(make-op :action 'take-arth-4520
			:add-list '(arth-4520))
		(make-op :action 'take-arth-4630
			:preconds '(arth-6430)
			:add-list '(arth-4630))
		(make-op :action 'take-arth-4710
			:add-list '(arth-4710))
		(make-op :action 'take-arth-4725
			:add-list '(arth-4725))
		(make-op :action 'take-arth-4730
			:preconds '(arth-2710 arth-2720)
			:add-list '(arth-4730))
		(make-op :action 'take-arth-4790
			:add-list '(arth-4790))
		(make-op :action 'take-arth-4800
			:add-list '(arth-4800))
		(make-op :action 'take-arth-4810
			:add-list '(arth-4810))
		(make-op :action 'take-arth-4900
			:add-list '(arth-4900))
		(make-op :action 'take-arth-4910
			:add-list '(arth-4910))
		(make-op :action 'take-arth-5730
			:add-list '(arth-5730))
		(make-op :action 'take-arth-5740
			:preconds '(rels-5740)
			:add-list '(arth-5740))
		(make-op :action 'take-auto-1000
			:add-list '(auto-1000))
		(make-op :action 'take-auto-1010
			:add-list '(auto-1010))
		(make-op :action 'take-auto-1020
			:add-list '(auto-1020))
		(make-op :action 'take-auto-1100
			:add-list '(auto-1100))
		(make-op :action 'take-auto-1105
			:add-list '(auto-1105))
		(make-op :action 'take-auto-1200
			:add-list '(auto-1200))
		(make-op :action 'take-auto-1205
			:add-list '(auto-1205))
		(make-op :action 'take-auto-1300
			:add-list '(auto-1300))
		(make-op :action 'take-auto-1305
			:add-list '(auto-1305))
		(make-op :action 'take-auto-1400
			:add-list '(auto-1400))
		(make-op :action 'take-auto-1405
			:add-list '(auto-1405))
		(make-op :action 'take-auto-1500
			:add-list '(auto-1500))
		(make-op :action 'take-auto-1505
			:add-list '(auto-1505))
		(make-op :action 'take-auto-1550
			:add-list '(auto-1550))
		(make-op :action 'take-auto-1600
			:add-list '(auto-1600))
		(make-op :action 'take-auto-1605
			:add-list '(auto-1605))
		(make-op :action 'take-auto-1800
			:add-list '(auto-1800))
		(make-op :action 'take-auto-1805
			:add-list '(auto-1805))
		(make-op :action 'take-auto-2600
			:add-list '(auto-2600))
		(make-op :action 'take-auto-2605
			:add-list '(auto-2605))
		(make-op :action 'take-auto-2700
			:add-list '(auto-2700))
		(make-op :action 'take-auto-2705
			:add-list '(auto-2705))
		(make-op :action 'take-auto-2800
			:add-list '(auto-2800))
		(make-op :action 'take-auto-2805
			:add-list '(auto-2805))
		(make-op :action 'take-auto-2977
			:add-list '(auto-2977))
		(make-op :action 'take-auto-2988
			:add-list '(auto-2988))
		(make-op :action 'take-av-1000
			:add-list '(av-1000))
		(make-op :action 'take-av-1100
			:add-list '(av-1100))
		(make-op :action 'take-av-1130
			:add-list '(av-1130))
		(make-op :action 'take-av-1140
			:add-list '(av-1140))
		(make-op :action 'take-av-1170
			:add-list '(av-1170))
		(make-op :action 'take-av-1240
			:preconds '(av-1130 av-1140 av-1170)
			:add-list '(av-1240))
		(make-op :action 'take-av-1900
			:add-list '(av-1900))
		(make-op :action 'take-av-1905
			:add-list '(av-1905))
		(make-op :action 'take-av-1910
			:preconds '(av-1905 av-1905 av-1905 av-1905)
			:add-list '(av-1910))
		(make-op :action 'take-av-1920
			:preconds '(av-1900 av-1905 av-1910)
			:add-list '(av-1920))
		(make-op :action 'take-av-1930
			:preconds '(av-1900 av-1905 av-1910 av-3500 av-3505)
			:add-list '(av-1930))
		(make-op :action 'take-av-1940
			:preconds '(av-1900 av-1905 av-1910)
			:add-list '(av-1940))
		(make-op :action 'take-av-1950
			:preconds '(av-1900 av-1905 av-1910)
			:add-list '(av-1950))
		(make-op :action 'take-av-1960
			:preconds '(av-1900 av-1905 av-1910)
			:add-list '(av-1960))
		(make-op :action 'take-av-1970
			:preconds '(av-1900 av-1905 av-1910)
			:add-list '(av-1970))
		(make-op :action 'take-av-1980
			:preconds '(av-1950)
			:add-list '(av-1980))
		(make-op :action 'take-av-1990
			:preconds '(av-1950)
			:add-list '(av-1990))
		(make-op :action 'take-av-2000
			:add-list '(av-2000))
		(make-op :action 'take-av-2050
			:add-list '(av-2050))
		(make-op :action 'take-av-2100
			:preconds '(av-2110)
			:add-list '(av-2100))
		(make-op :action 'take-av-2110
			:preconds '(av-2100 av-2100)
			:add-list '(av-2110))
		(make-op :action 'take-av-2140
			:preconds '(av-2100 av-2150)
			:add-list '(av-2140))
		(make-op :action 'take-av-2150
			:preconds '(av-2110 av-2140)
			:add-list '(av-2150))
		(make-op :action 'take-av-2160
			:add-list '(av-2160))
		(make-op :action 'take-av-2170
			:preconds '(av-2190)
			:add-list '(av-2170))
		(make-op :action 'take-av-2175
			:preconds '(av-2335)
			:add-list '(av-2175))
		(make-op :action 'take-av-2180
			:preconds '(av-2200)
			:add-list '(av-2180))
		(make-op :action 'take-av-2190
			:preconds '(av-2170 av-2170)
			:add-list '(av-2190))
		(make-op :action 'take-av-2200
			:preconds '(av-2180 av-2180)
			:add-list '(av-2200))
		(make-op :action 'take-av-2250
			:add-list '(av-2250))
		(make-op :action 'take-av-2300
			:preconds '(math-1050)
			:add-list '(av-2300))
		(make-op :action 'take-av-2320
			:add-list '(av-2320))
		(make-op :action 'take-av-2330
			:preconds '(av-2350)
			:add-list '(av-2330))
		(make-op :action 'take-av-2335
			:preconds '(av-2355)
			:add-list '(av-2335))
		(make-op :action 'take-av-2350
			:preconds '(av-2330)
			:add-list '(av-2350))
		(make-op :action 'take-av-2355
			:preconds '(av-2335)
			:add-list '(av-2355))
		(make-op :action 'take-av-2365
			:preconds '(av-2355)
			:add-list '(av-2365))
		(make-op :action 'take-av-2401
			:preconds '(av-2350 av-2450 av-2451)
			:add-list '(av-2401))
		(make-op :action 'take-av-2430
			:preconds '(tee-2300)
			:add-list '(av-2430))
		(make-op :action 'take-av-2440
			:preconds '(av-2430 tee-2300 av-2430)
			:add-list '(av-2440))
		(make-op :action 'take-av-2450
			:preconds '(av-2350 av-2401)
			:add-list '(av-2450))
		(make-op :action 'take-av-2451
			:preconds '(av-2350 av-2401)
			:add-list '(av-2451))
		(make-op :action 'take-av-2500
			:preconds '(av-2330 av-2350)
			:add-list '(av-2500))
		(make-op :action 'take-av-2504
			:preconds '(av-2330 av-2350)
			:add-list '(av-2504))
		(make-op :action 'take-av-2505
			:preconds '(av-2330 av-2350)
			:add-list '(av-2505))
		(make-op :action 'take-av-2520
			:preconds '(av-2350 av-2450 av-2451 av-2540)
			:add-list '(av-2520))
		(make-op :action 'take-av-2525
			:preconds '(av-2365)
			:add-list '(av-2525))
		(make-op :action 'take-av-2530
			:preconds '(av-2320 av-2330)
			:add-list '(av-2530))
		(make-op :action 'take-av-2540
			:preconds '(av-2350 av-2450 av-2451 av-2520)
			:add-list '(av-2540))
		(make-op :action 'take-av-2545
			:preconds '(av-2365)
			:add-list '(av-2545))
		(make-op :action 'take-av-2555
			:preconds '(av-2545 av-2545)
			:add-list '(av-2555))
		(make-op :action 'take-av-2620
			:preconds '(av-2540)
			:add-list '(av-2620))
		(make-op :action 'take-av-2625
			:preconds '(av-2545 av-2665)
			:add-list '(av-2625))
		(make-op :action 'take-av-2630
			:preconds '(av-2330 av-2320)
			:add-list '(av-2630))
		(make-op :action 'take-av-2660
			:preconds '(av-2450 av-2451 av-2540)
			:add-list '(av-2660))
		(make-op :action 'take-av-2665
			:preconds '(av-2365)
			:add-list '(av-2665))
		(make-op :action 'take-av-2670
			:preconds '(av-2660)
			:add-list '(av-2670))
		(make-op :action 'take-av-2675
			:preconds '(av-2665 av-2625)
			:add-list '(av-2675))
		(make-op :action 'take-av-2870
			:preconds '(av-2670)
			:add-list '(av-2870))
		(make-op :action 'take-av-3010
			:add-list '(av-3010))
		(make-op :action 'take-av-3100
			:add-list '(av-3100))
		(make-op :action 'take-av-3120
			:add-list '(av-3120))
		(make-op :action 'take-av-3140
			:preconds '(av-2540)
			:add-list '(av-3140))
		(make-op :action 'take-av-3145
			:preconds '(av-2545)
			:add-list '(av-3145))
		(make-op :action 'take-av-3280
			:preconds '(av-2140 av-2150 av-2170 av-2190 av-2200 av-2440 av-3420 av-4200 phys-1800)
			:add-list '(av-3280))
		(make-op :action 'take-av-3410
			:preconds '(av-2540 av-2620 av-2660)
			:add-list '(av-3410))
		(make-op :action 'take-av-3420
			:preconds '(av-1140)
			:add-list '(av-3420))
		(make-op :action 'take-av-3440
			:preconds '(av-2540 av-2620)
			:add-list '(av-3440))
		(make-op :action 'take-av-3500
			:add-list '(av-3500))
		(make-op :action 'take-av-3505
			:add-list '(av-3505))
		(make-op :action 'take-av-3510
			:preconds '(av-1900 av-1905 av-1910 av-3500 av-3505)
			:add-list '(av-3510))
		(make-op :action 'take-av-3515
			:preconds '(av-1900 av-1905 av-1910 av-3500 av-3505)
			:add-list '(av-3515))
		(make-op :action 'take-av-3520
			:preconds '(av-3515)
			:add-list '(av-3520))
		(make-op :action 'take-av-3525
			:preconds '(av-3515 av-3520)
			:add-list '(av-3525))
		(make-op :action 'take-av-3530
			:preconds '(av-3500 av-3505)
			:add-list '(av-3530))
		(make-op :action 'take-av-3535
			:preconds '(av-1900 av-1905 av-1910 av-3500 av-3505)
			:add-list '(av-3535))
		(make-op :action 'take-av-3550
			:preconds '(av-3515 av-3535)
			:add-list '(av-3550))
		(make-op :action 'take-av-3560
			:preconds '(av-3500 av-3505)
			:add-list '(av-3560))
		(make-op :action 'take-av-3590
			:add-list '(av-3590))
		(make-op :action 'take-av-3720
			:preconds '(av-2670 av-3410)
			:add-list '(av-3720))
		(make-op :action 'take-av-3725
			:preconds '(av-2545 av-2675)
			:add-list '(av-3725))
		(make-op :action 'take-av-3735
			:preconds '(av-2675 av-3725)
			:add-list '(av-3735))
		(make-op :action 'take-av-3740
			:preconds '(av-2670 av-3410 av-3720)
			:add-list '(av-3740))
		(make-op :action 'take-av-3760
			:preconds '(av-3720 av-3740 av-3740)
			:add-list '(av-3760))
		(make-op :action 'take-av-3825
			:preconds '(av-3725 av-3735)
			:add-list '(av-3825))
		(make-op :action 'take-av-3835
			:preconds '(av-3735 av-3825)
			:add-list '(av-3835))
		(make-op :action 'take-av-3880
			:preconds '(av-3740)
			:add-list '(av-3880))
		(make-op :action 'take-av-3885
			:preconds '(av-2625)
			:add-list '(av-3885))
		(make-op :action 'take-av-3900
			:add-list '(av-3900))
		(make-op :action 'take-av-4100
			:add-list '(av-4100))
		(make-op :action 'take-av-4110
			:add-list '(av-4110))
		(make-op :action 'take-av-4150
			:preconds '(av-1140 av-1240 av-2170)
			:add-list '(av-4150))
		(make-op :action 'take-av-4200
			:preconds '(av-1170)
			:add-list '(av-4200))
		(make-op :action 'take-av-4250
			:add-list '(av-4250))
		(make-op :action 'take-av-4280
			:add-list '(av-4280))
		(make-op :action 'take-av-4300
			:add-list '(av-4300))
		(make-op :action 'take-av-4490
			:add-list '(av-4490))
		(make-op :action 'take-av-4505
			:preconds '(av-2175 av-2355)
			:add-list '(av-4505))
		(make-op :action 'take-av-4605
			:preconds '(av-2365)
			:add-list '(av-4605))
		(make-op :action 'take-av-4610
			:add-list '(av-4610))
		(make-op :action 'take-av-4620
			:preconds '(av-4610)
			:add-list '(av-4620))
		(make-op :action 'take-av-4660
			:add-list '(av-4660))
		(make-op :action 'take-av-4710
			:add-list '(av-4710))
		(make-op :action 'take-av-4720
			:add-list '(av-4720))
		(make-op :action 'take-av-4750
			:add-list '(av-4750))
		(make-op :action 'take-av-4880
			:preconds '(av-3880)
			:add-list '(av-4880))
		(make-op :action 'take-av-4930
			:add-list '(av-4930))
		(make-op :action 'take-av-5150
			:preconds '(av-6150)
			:add-list '(av-5150))
		(make-op :action 'take-av-5160
			:add-list '(av-5160))
		(make-op :action 'take-av-5300
			:preconds '(av-3880)
			:add-list '(av-5300))
		(make-op :action 'take-av-5400
			:preconds '(av-2670 av-3410)
			:add-list '(av-5400))
		(make-op :action 'take-av-5420
			:preconds '(av-5400)
			:add-list '(av-5420))
		(make-op :action 'take-av-5500
			:preconds '(av-2670 av-3880)
			:add-list '(av-5500))
		(make-op :action 'take-av-5760
			:add-list '(av-5760))
		(make-op :action 'take-av-5910
			:add-list '(av-5910))
		(make-op :action 'take-beng-1000
			:add-list '(beng-1000))
		(make-op :action 'take-beng-1200
			:add-list '(beng-1200))
		(make-op :action 'take-beng-1880
			:add-list '(beng-1880))
		(make-op :action 'take-beng-2290
			:add-list '(beng-2290))
		(make-op :action 'take-beng-2330
			:preconds '(biol-1610 chem-2300 chem-2310 chem-2315 phys-2210 phys-2215)
			:add-list '(beng-2330))
		(make-op :action 'take-beng-2400
			:preconds '(math-1220)
			:add-list '(beng-2400))
		(make-op :action 'take-beng-3000
			:preconds '(engr-2210 tee-2300)
			:add-list '(beng-3000))
		(make-op :action 'take-beng-3200
			:preconds '(chem-2300 chem-2310 chem-2315)
			:add-list '(beng-3200))
		(make-op :action 'take-beng-3500
			:preconds '(beng-2400 math-2250 math-2270)
			:add-list '(beng-3500))
		(make-op :action 'take-beng-3670
			:preconds '(beng-2400 mae-2300 beng-3500)
			:add-list '(beng-3670))
		(make-op :action 'take-beng-3870
			:add-list '(beng-3870))
		(make-op :action 'take-beng-4250
			:add-list '(beng-4250))
		(make-op :action 'take-beng-4880
			:preconds '(beng-3870)
			:add-list '(beng-4880))
		(make-op :action 'take-beng-4890
			:preconds '(beng-4880)
			:add-list '(beng-4890))
		(make-op :action 'take-beng-4930
			:add-list '(beng-4930))
		(make-op :action 'take-beng-5020
			:preconds '(beng-3670 cee-3670 engr-2450 mae-2450 math-2250 math-2270 math-2280)
			:add-list '(beng-5020))
		(make-op :action 'take-beng-5500
			:preconds '(math-2250 math-2270 cee-2450 cs-1400 beng-6500)
			:add-list '(beng-5500))
		(make-op :action 'take-beng-5600
			:preconds '(beng-3670 cee-3670 beng-6600)
			:add-list '(beng-5600))
		(make-op :action 'take-beng-5610
			:preconds '(beng-3200 ndfs-5610 beng-6610)
			:add-list '(beng-5610))
		(make-op :action 'take-beng-5620
			:preconds '(chem-3700 math-2250 beng-6620)
			:add-list '(beng-5620))
		(make-op :action 'take-beng-5630
			:preconds '(beng-6630)
			:add-list '(beng-5630))
		(make-op :action 'take-beng-5640
			:add-list '(beng-5640))
		(make-op :action 'take-beng-5660
			:add-list '(beng-5660))
		(make-op :action 'take-beng-5680
			:preconds '(cee-5680 beng-6680)
			:add-list '(beng-5680))
		(make-op :action 'take-beng-5700
			:preconds '(phys-2210 phys-2310 phys-2215 phys-2220 phys-2320 phys-2225 ece-5700 mae-5700 phys-5700)
			:add-list '(beng-5700))
		(make-op :action 'take-beng-5710
			:preconds '(beng-5700 ece-5700 mae-5700 phys-5700 ece-5710 phys-5710)
			:add-list '(beng-5710))
		(make-op :action 'take-beng-5790
			:preconds '(biol-5790 chem-5790 beng-6790)
			:add-list '(beng-5790))
		(make-op :action 'take-beng-5810
			:preconds '(beng-3200 beng-3670 cee-3670 beng-3670 cee-3670 cee-3610 pubh-3610 cee-3640 cee-5810 beng-6810)
			:add-list '(beng-5810))
		(make-op :action 'take-beng-5830
			:preconds '(beng-3200 beng-3670 cee-3670 cee-3610 pubh-3610 cee-3640 cee-5830 beng-6830)
			:add-list '(beng-5830))
		(make-op :action 'take-beng-5840
			:preconds '(math-2250 phys-2210 beng-6840)
			:add-list '(beng-5840))
		(make-op :action 'take-beng-5850
			:preconds '(beng-2330 beng-5020 beng-6850)
			:add-list '(beng-5850))
		(make-op :action 'take-beng-5870
			:preconds '(beng-3670 cee-3670 engr-2450 mae-2450 math-2250 math-2270 math-2280 beng-6870)
			:add-list '(beng-5870))
		(make-op :action 'take-beng-5880
			:preconds '(beng-3500 beng-6880)
			:add-list '(beng-5880))
		(make-op :action 'take-beng-5890
			:preconds '(beng-2330 beng-6890)
			:add-list '(beng-5890))
		(make-op :action 'take-beng-5910
			:preconds '(beng-2330 beng-6910)
			:add-list '(beng-5910))
		(make-op :action 'take-beng-5930
			:add-list '(beng-5930))
		(make-op :action 'take-biol-1010
			:add-list '(biol-1010))
		(make-op :action 'take-biol-1013
			:add-list '(biol-1013))
		(make-op :action 'take-biol-1030
			:add-list '(biol-1030))
		(make-op :action 'take-biol-1040
			:add-list '(biol-1040))
		(make-op :action 'take-biol-1050
			:add-list '(biol-1050))
		(make-op :action 'take-biol-1060
			:add-list '(biol-1060))
		(make-op :action 'take-biol-1065
			:preconds '(biol-1060)
			:add-list '(biol-1065))
		(make-op :action 'take-biol-1410
			:add-list '(biol-1410))
		(make-op :action 'take-biol-1415
			:add-list '(biol-1415))
		(make-op :action 'take-biol-1610
			:preconds '(biol-1615 biol-1615 biol-1620 biol-1625 biol-3300 biol-1615 biol-3300)
			:add-list '(biol-1610))
		(make-op :action 'take-biol-1615
			:add-list '(biol-1615))
		(make-op :action 'take-biol-1620
			:preconds '(biol-1625 biol-1625 biol-1610)
			:add-list '(biol-1620))
		(make-op :action 'take-biol-1625
			:add-list '(biol-1625))
		(make-op :action 'take-biol-1750
			:add-list '(biol-1750))
		(make-op :action 'take-biol-2030
			:preconds '(biol-1610 chem-1010 biol-2035)
			:add-list '(biol-2030))
		(make-op :action 'take-biol-2035
			:add-list '(biol-2035))
		(make-op :action 'take-biol-2060
			:preconds '(usu-1350 biol-1010 biol-1610 biol-2320 biol-2420 chem-1010 chem-1110 chem-1210 heal-1500)
			:add-list '(biol-2060))
		(make-op :action 'take-biol-2220
			:preconds '(biol-1610 biol-1620 biol-1620 wats-2220)
			:add-list '(biol-2220))
		(make-op :action 'take-biol-2320
			:add-list '(biol-2320))
		(make-op :action 'take-biol-2420
			:preconds '(biol-1010 biol-1610 biol-2060 biol-2320 chem-1010 chem-1110 chem-1210 usu-1350)
			:add-list '(biol-2420))
		(make-op :action 'take-biol-2520
			:preconds '(biol-2320 biol-2420)
			:add-list '(biol-2520))
		(make-op :action 'take-biol-2700
			:add-list '(biol-2700))
		(make-op :action 'take-biol-2800
			:add-list '(biol-2800))
		(make-op :action 'take-biol-2977
			:add-list '(biol-2977))
		(make-op :action 'take-biol-2988
			:add-list '(biol-2988))
		(make-op :action 'take-biol-3010
			:add-list '(biol-3010))
		(make-op :action 'take-biol-3020
			:preconds '(biol-1610 biol-1615 biol-1620 biol-1625)
			:add-list '(biol-3020))
		(make-op :action 'take-biol-3030
			:preconds '(biol-1010 biol-1610 usu-1350 honr-1350 biol-3060)
			:add-list '(biol-3030))
		(make-op :action 'take-biol-3040
			:add-list '(biol-3040))
		(make-op :action 'take-biol-3050
			:preconds '(biol-1610 biol-1620 chem-1110 chem-1210)
			:add-list '(biol-3050))
		(make-op :action 'take-biol-3055
			:preconds '(biol-1610 biol-1620 biol-1615 biol-1625 biol-3050 chem-1210 stat-2000 stat-3000)
			:add-list '(biol-3055))
		(make-op :action 'take-biol-3060
			:preconds '(biol-1610 chem-1110 chem-1210 biol-1610)
			:add-list '(biol-3060))
		(make-op :action 'take-biol-3065
			:preconds '(biol-1615 biol-1625 biol-3060)
			:add-list '(biol-3065))
		(make-op :action 'take-biol-3070
			:preconds '(biol-1610 biol-1620 stat-2000 stat-3000 math-1060 math-1210)
			:add-list '(biol-3070))
		(make-op :action 'take-biol-3085
			:preconds '(biol-1610 biol-1620 biol-1615 biol-1625 biol-2220 math-1100 math-1210 stat-2000 stat-3000)
			:add-list '(biol-3085))
		(make-op :action 'take-biol-3090
			:preconds '(biol-1620 chem-1210 chem-1120)
			:add-list '(biol-3090))
		(make-op :action 'take-biol-3095
			:preconds '(biol-1615 biol-1625 biol-3090)
			:add-list '(biol-3095))
		(make-op :action 'take-biol-3100
			:add-list '(biol-3100))
		(make-op :action 'take-biol-3200
			:preconds '(biol-1620 biol-1625)
			:add-list '(biol-3200))
		(make-op :action 'take-biol-3220
			:preconds '(biol-2220 math-1100 math-1210)
			:add-list '(biol-3220))
		(make-op :action 'take-biol-3300
			:preconds '(biol-1610 biol-1615 chem-1120 chem-1220 chem-2300 chem-2310 biol-1610 biol-1610)
			:add-list '(biol-3300))
		(make-op :action 'take-biol-3500
			:add-list '(biol-3500))
		(make-op :action 'take-biol-3760
			:preconds '(biol-1620)
			:add-list '(biol-3760))
		(make-op :action 'take-biol-4000
			:preconds '(biol-2320)
			:add-list '(biol-4000))
		(make-op :action 'take-biol-4030
			:preconds '(biol-1610 biol-2060 biol-2420 pubh-4030)
			:add-list '(biol-4030))
		(make-op :action 'take-biol-4040
			:preconds '(pubh-4040)
			:add-list '(biol-4040))
		(make-op :action 'take-biol-4050
			:preconds '(biol-3070 biol-6050)
			:add-list '(biol-4050))
		(make-op :action 'take-biol-4060
			:preconds '(biol-1620 biol-1625 biol-2220 wats-2220)
			:add-list '(biol-4060))
		(make-op :action 'take-biol-4230
			:preconds '(math-2280 math-2270 biol-4270 math-4230)
			:add-list '(biol-4230))
		(make-op :action 'take-biol-4250
			:add-list '(biol-4250))
		(make-op :action 'take-biol-4270
			:preconds '(biol-1610 biol-2220 math-1210)
			:add-list '(biol-4270))
		(make-op :action 'take-biol-4300
			:preconds '(stat-3000 biol-6300)
			:add-list '(biol-4300))
		(make-op :action 'take-biol-4310
			:preconds '(biol-1620 math-1210 stat-3000)
			:add-list '(biol-4310))
		(make-op :action 'take-biol-4320
			:preconds '(biol-1620 math-1210 stat-3000)
			:add-list '(biol-4320))
		(make-op :action 'take-biol-4400
			:preconds '(biol-1620 biol-1625 math-1050)
			:add-list '(biol-4400))
		(make-op :action 'take-biol-4410
			:preconds '(biol-1610 biol-1615)
			:add-list '(biol-4410))
		(make-op :action 'take-biol-4420
			:preconds '(biol-1620 biol-6420)
			:add-list '(biol-4420))
		(make-op :action 'take-biol-4430
			:preconds '(biol-1610 biol-1615)
			:add-list '(biol-4430))
		(make-op :action 'take-biol-4450
			:preconds '(biol-1620 chem-1220)
			:add-list '(biol-4450))
		(make-op :action 'take-biol-4500
			:preconds '(biol-1610 biol-1615 biol-1620 biol-1625)
			:add-list '(biol-4500))
		(make-op :action 'take-biol-4540
			:preconds '(biol-1620 biol-6540)
			:add-list '(biol-4540))
		(make-op :action 'take-biol-4600
			:preconds '(biol-1620 biol-1625 biol-2320)
			:add-list '(biol-4600))
		(make-op :action 'take-biol-4650
			:preconds '(biol-1620 biol-6650)
			:add-list '(biol-4650))
		(make-op :action 'take-biol-4710
			:add-list '(biol-4710))
		(make-op :action 'take-biol-4750
			:add-list '(biol-4750))
		(make-op :action 'take-biol-4770
			:preconds '(math-1100 math-1210 biol-1610 chem-1210 biol-6770)
			:add-list '(biol-4770))
		(make-op :action 'take-biol-5010
			:preconds '(biol-1620 biol-6010)
			:add-list '(biol-5010))
		(make-op :action 'take-biol-5110
			:preconds '(biol-1610 biol-1620 chem-1210 chem-1220 advs-5110 biol-6110)
			:add-list '(biol-5110))
		(make-op :action 'take-biol-5150
			:preconds '(chem-1220 biol-3060 biol-3300 biol-5210)
			:add-list '(biol-5150))
		(make-op :action 'take-biol-5160
			:preconds '(advs-5160 ndfs-5160 psc-5160)
			:add-list '(biol-5160))
		(make-op :action 'take-biol-5190
			:preconds '(biol-3060 chem-3700 chem-5700 biol-6190)
			:add-list '(biol-5190))
		(make-op :action 'take-biol-5210
			:preconds '(biol-1620 biol-3060 chem-2300 chem-2320 chem-3700 chem-5700)
			:add-list '(biol-5210))
		(make-op :action 'take-biol-5220
			:preconds '(chem-3700 advs-5220 ndfs-5220 biol-6220)
			:add-list '(biol-5220))
		(make-op :action 'take-biol-5230
			:preconds '(biol-3060 biol-5210 chem-3700 chem-5700)
			:add-list '(biol-5230))
		(make-op :action 'take-biol-5240
			:preconds '(biol-3060)
			:add-list '(biol-5240))
		(make-op :action 'take-biol-5260
			:preconds '(chem-3700 chem-5710 biol-3060 advs-5260 ndfs-5260 psc-5260)
			:add-list '(biol-5260))
		(make-op :action 'take-biol-5280
			:preconds '(advs-5260 advs-5280 biol-6280)
			:add-list '(biol-5280))
		(make-op :action 'take-biol-5300
			:preconds '(biol-3300 math-1210)
			:add-list '(biol-5300))
		(make-op :action 'take-biol-5310
			:preconds '(biol-1610 biol-1615 biol-1620 biol-1625 chem-1220 psc-3000 psc-5310)
			:add-list '(biol-5310))
		(make-op :action 'take-biol-5323
			:preconds '(biol-6323)
			:add-list '(biol-5323))
		(make-op :action 'take-biol-5390
			:preconds '(engl-2010 biol-3060 wild-4880 biol-6390)
			:add-list '(biol-5390))
		(make-op :action 'take-biol-5400
			:preconds '(chem-1220 biol-1620 chem-2300 advs-5400 pubh-5400 biol-6400)
			:add-list '(biol-5400))
		(make-op :action 'take-biol-5530
			:preconds '(biol-1620)
			:add-list '(biol-5530))
		(make-op :action 'take-biol-5550
			:preconds '(wats-5550)
			:add-list '(biol-5550))
		(make-op :action 'take-biol-5560
			:preconds '(biol-1620 biol-1625)
			:add-list '(biol-5560))
		(make-op :action 'take-biol-5570
			:preconds '(biol-1620 biol-1625)
			:add-list '(biol-5570))
		(make-op :action 'take-biol-5580
			:preconds '(biol-1620 biol-1625 wild-5580)
			:add-list '(biol-5580))
		(make-op :action 'take-biol-5600
			:preconds '(biol-1620 biol-1625 chem-1110 chem-1120 chem-1220 biol-6600)
			:add-list '(biol-5600))
		(make-op :action 'take-biol-5610
			:preconds '(biol-1620 biol-1625 biol-2420 biol-4450 biol-5600)
			:add-list '(biol-5610))
		(make-op :action 'take-biol-5625
			:preconds '(biol-1610 biol-1615 biol-1620 biol-1625 biol-4450)
			:add-list '(biol-5625))
		(make-op :action 'take-biol-5630
			:add-list '(biol-5630))
		(make-op :action 'take-biol-5790
			:preconds '(beng-5790 chem-5790 biol-6790)
			:add-list '(biol-5790))
		(make-op :action 'take-biol-5800
			:preconds '(biol-1620 biol-1625)
			:add-list '(biol-5800))
		(make-op :action 'take-biol-5810
			:preconds '(biol-5800)
			:add-list '(biol-5810))
		(make-op :action 'take-biol-5850
			:preconds '(biol-6850)
			:add-list '(biol-5850))
		(make-op :action 'take-bccm-1010
			:add-list '(bccm-1010))
		(make-op :action 'take-bccm-1150
			:add-list '(bccm-1150))
		(make-op :action 'take-bccm-1250
			:add-list '(bccm-1250))
		(make-op :action 'take-bccm-2010
			:add-list '(bccm-2010))
		(make-op :action 'take-bccm-2015
			:add-list '(bccm-2015))
		(make-op :action 'take-bccm-2020
			:preconds '(bccm-1010 bccm-2010)
			:add-list '(bccm-2020))
		(make-op :action 'take-bccm-2030
			:preconds '(bccm-2010)
			:add-list '(bccm-2030))
		(make-op :action 'take-bccm-2080
			:add-list '(bccm-2080))
		(make-op :action 'take-bccm-2090
			:preconds '(bccm-2080)
			:add-list '(bccm-2090))
		(make-op :action 'take-bccm-2100
			:add-list '(bccm-2100))
		(make-op :action 'take-bccm-2110
			:preconds '(bccm-2100)
			:add-list '(bccm-2110))
		(make-op :action 'take-bccm-2120
			:add-list '(bccm-2120))
		(make-op :action 'take-bccm-2125
			:add-list '(bccm-2125))
		(make-op :action 'take-bccm-2130
			:add-list '(bccm-2130))
		(make-op :action 'take-bccm-2170
			:add-list '(bccm-2170))
		(make-op :action 'take-bccm-2180
			:preconds '(bccm-2170)
			:add-list '(bccm-2180))
		(make-op :action 'take-bccm-2200
			:add-list '(bccm-2200))
		(make-op :action 'take-bccm-2230
			:add-list '(bccm-2230))
		(make-op :action 'take-bccm-2240
			:add-list '(bccm-2240))
		(make-op :action 'take-bccm-2250
			:add-list '(bccm-2250))
		(make-op :action 'take-bccm-2270
			:add-list '(bccm-2270))
		(make-op :action 'take-bccm-2320
			:add-list '(bccm-2320))
		(make-op :action 'take-bccm-2340
			:add-list '(bccm-2340))
		(make-op :action 'take-bccm-2350
			:add-list '(bccm-2350))
		(make-op :action 'take-bccm-2500
			:add-list '(bccm-2500))
		(make-op :action 'take-bccm-2600
			:add-list '(bccm-2600))
		(make-op :action 'take-bccm-2977
			:add-list '(bccm-2977))
		(make-op :action 'take-bccm-2988
			:add-list '(bccm-2988))
		(make-op :action 'take-busn-1010
			:add-list '(busn-1010))
		(make-op :action 'take-busn-1020
			:add-list '(busn-1020))
		(make-op :action 'take-busn-1021
			:add-list '(busn-1021))
		(make-op :action 'take-busn-1050
			:add-list '(busn-1050))
		(make-op :action 'take-busn-1091
			:add-list '(busn-1091))
		(make-op :action 'take-busn-1111
			:add-list '(busn-1111))
		(make-op :action 'take-busn-1310
			:add-list '(busn-1310))
		(make-op :action 'take-busn-1400
			:add-list '(busn-1400))
		(make-op :action 'take-busn-1405
			:add-list '(busn-1405))
		(make-op :action 'take-busn-1410
			:add-list '(busn-1410))
		(make-op :action 'take-busn-1500
			:add-list '(busn-1500))
		(make-op :action 'take-busn-2010
			:add-list '(busn-2010))
		(make-op :action 'take-busn-2020
			:preconds '(math-1030)
			:add-list '(busn-2020))
		(make-op :action 'take-busn-2050
			:add-list '(busn-2050))
		(make-op :action 'take-busn-2100
			:add-list '(busn-2100))
		(make-op :action 'take-busn-2151
			:add-list '(busn-2151))
		(make-op :action 'take-busn-2160
			:preconds '(busn-2010)
			:add-list '(busn-2160))
		(make-op :action 'take-busn-2200
			:add-list '(busn-2200))
		(make-op :action 'take-busn-2201
			:add-list '(busn-2201))
		(make-op :action 'take-busn-2202
			:add-list '(busn-2202))
		(make-op :action 'take-busn-2211
			:preconds '(busn-2201)
			:add-list '(busn-2211))
		(make-op :action 'take-busn-2241
			:add-list '(busn-2241))
		(make-op :action 'take-busn-2251
			:add-list '(busn-2251))
		(make-op :action 'take-busn-2320
			:add-list '(busn-2320))
		(make-op :action 'take-busn-2321
			:add-list '(busn-2321))
		(make-op :action 'take-busn-2390
			:add-list '(busn-2390))
		(make-op :action 'take-busn-2420
			:add-list '(busn-2420))
		(make-op :action 'take-busn-2430
			:add-list '(busn-2430))
		(make-op :action 'take-busn-2451
			:add-list '(busn-2451))
		(make-op :action 'take-busn-2590
			:add-list '(busn-2590))
		(make-op :action 'take-busn-2600
			:add-list '(busn-2600))
		(make-op :action 'take-busn-2620
			:add-list '(busn-2620))
		(make-op :action 'take-busn-2700
			:add-list '(busn-2700))
		(make-op :action 'take-busn-2710
			:add-list '(busn-2710))
		(make-op :action 'take-busn-2800
			:add-list '(busn-2800))
		(make-op :action 'take-busn-2977
			:add-list '(busn-2977))
		(make-op :action 'take-busn-2988
			:add-list '(busn-2988))
		(make-op :action 'take-busn-2999
			:add-list '(busn-2999))
		(make-op :action 'take-tebp-1000
			:add-list '(tebp-1000))
		(make-op :action 'take-tebp-1005
			:add-list '(tebp-1005))
		(make-op :action 'take-tebp-1100
			:add-list '(tebp-1100))
		(make-op :action 'take-tebp-1130
			:add-list '(tebp-1130))
		(make-op :action 'take-tebp-1140
			:add-list '(tebp-1140))
		(make-op :action 'take-tebp-1150
			:add-list '(tebp-1150))
		(make-op :action 'take-tebp-1200
			:add-list '(tebp-1200))
		(make-op :action 'take-tebp-1300
			:add-list '(tebp-1300))
		(make-op :action 'take-tebp-1350
			:add-list '(tebp-1350))
		(make-op :action 'take-tebp-1500
			:add-list '(tebp-1500))
		(make-op :action 'take-tebp-1550
			:add-list '(tebp-1550))
		(make-op :action 'take-tebp-1650
			:add-list '(tebp-1650))
		(make-op :action 'take-tebp-1700
			:add-list '(tebp-1700))
		(make-op :action 'take-tebp-1840
			:add-list '(tebp-1840))
		(make-op :action 'take-tebp-2190
			:add-list '(tebp-2190))
		(make-op :action 'take-bcis-1000
			:add-list '(bcis-1000))
		(make-op :action 'take-bcis-1040
			:add-list '(bcis-1040))
		(make-op :action 'take-bcis-1050
			:add-list '(bcis-1050))
		(make-op :action 'take-bcis-1081
			:add-list '(bcis-1081))
		(make-op :action 'take-bcis-1100
			:add-list '(bcis-1100))
		(make-op :action 'take-bcis-1200
			:add-list '(bcis-1200))
		(make-op :action 'take-bcis-1250
			:preconds '(bcis-1260)
			:add-list '(bcis-1250))
		(make-op :action 'take-bcis-1260
			:preconds '(bcis-1250)
			:add-list '(bcis-1260))
		(make-op :action 'take-bcis-1300
			:add-list '(bcis-1300))
		(make-op :action 'take-bcis-1310
			:add-list '(bcis-1310))
		(make-op :action 'take-bcis-1320
			:add-list '(bcis-1320))
		(make-op :action 'take-bcis-1340
			:add-list '(bcis-1340))
		(make-op :action 'take-bcis-1350
			:add-list '(bcis-1350))
		(make-op :action 'take-bcis-1400
			:add-list '(bcis-1400))
		(make-op :action 'take-bcis-1411
			:add-list '(bcis-1411))
		(make-op :action 'take-bcis-1500
			:add-list '(bcis-1500))
		(make-op :action 'take-bcis-1540
			:add-list '(bcis-1540))
		(make-op :action 'take-bcis-2010
			:add-list '(bcis-2010))
		(make-op :action 'take-bcis-2210
			:add-list '(bcis-2210))
		(make-op :action 'take-bcis-2300
			:add-list '(bcis-2300))
		(make-op :action 'take-bcis-2330
			:add-list '(bcis-2330))
		(make-op :action 'take-bcis-2340
			:add-list '(bcis-2340))
		(make-op :action 'take-bcis-2345
			:add-list '(bcis-2345))
		(make-op :action 'take-bcis-2346
			:add-list '(bcis-2346))
		(make-op :action 'take-bcis-2441
			:add-list '(bcis-2441))
		(make-op :action 'take-bcis-2500
			:preconds '(bcis-1300)
			:add-list '(bcis-2500))
		(make-op :action 'take-bcis-2631
			:add-list '(bcis-2631))
		(make-op :action 'take-bcis-2632
			:add-list '(bcis-2632))
		(make-op :action 'take-bcis-2650
			:add-list '(bcis-2650))
		(make-op :action 'take-bcis-2651
			:add-list '(bcis-2651))
		(make-op :action 'take-bcis-2652
			:add-list '(bcis-2652))
		(make-op :action 'take-bcis-2653
			:add-list '(bcis-2653))
		(make-op :action 'take-bcis-2654
			:add-list '(bcis-2654))
		(make-op :action 'take-bcis-2655
			:add-list '(bcis-2655))
		(make-op :action 'take-bcis-2930
			:add-list '(bcis-2930))
		(make-op :action 'take-bcis-2977
			:add-list '(bcis-2977))
		(make-op :action 'take-bcis-2988
			:add-list '(bcis-2988))
		(make-op :action 'take-bcis-2999
			:add-list '(bcis-2999))
		(make-op :action 'take-bsed-2510
			:add-list '(bsed-2510))
		(make-op :action 'take-bsed-3150
			:preconds '(bsed-2510)
			:add-list '(bsed-3150))
		(make-op :action 'take-bsed-3300
			:add-list '(bsed-3300))
		(make-op :action 'take-bsed-4150
			:preconds '(bsed-2510)
			:add-list '(bsed-4150))
		(make-op :action 'take-bsed-4210
			:preconds '(aste-4210 fcse-4210 tee-4210)
			:add-list '(bsed-4210))
		(make-op :action 'take-bsed-4300
			:add-list '(bsed-4300))
		(make-op :action 'take-bsed-5500
			:preconds '(bsed-4150 bsed-4300 bsed-5630)
			:add-list '(bsed-5500))
		(make-op :action 'take-bsed-5630
			:preconds '(bsed-4150 bsed-4300 bsed-5500)
			:add-list '(bsed-5630))
		(make-op :action 'take-bhis-1010
			:preconds '(heal-1008 bhis-1015)
			:add-list '(bhis-1010))
		(make-op :action 'take-bhis-1015
			:preconds '(bhis-1010 heal-1008 bhis-1010)
			:add-list '(bhis-1015))
		(make-op :action 'take-bhis-1020
			:add-list '(bhis-1020))
		(make-op :action 'take-bhis-1025
			:add-list '(bhis-1025))
		(make-op :action 'take-bhis-2010
			:add-list '(bhis-2010))
		(make-op :action 'take-bhis-2015
			:add-list '(bhis-2015))
		(make-op :action 'take-bhis-2020
			:add-list '(bhis-2020))
		(make-op :action 'take-bhis-2025
			:add-list '(bhis-2025))
		(make-op :action 'take-bhis-2030
			:add-list '(bhis-2030))
		(make-op :action 'take-bhis-2035
			:add-list '(bhis-2035))
		(make-op :action 'take-bhis-2040
			:add-list '(bhis-2040))
		(make-op :action 'take-bhis-2988
			:add-list '(bhis-2988))
		(make-op :action 'take-bhis-2999
			:add-list '(bhis-2999))
		(make-op :action 'take-bus-1000
			:add-list '(bus-1000))
		(make-op :action 'take-bus-1500
			:add-list '(bus-1500))
		(make-op :action 'take-bus-1600
			:add-list '(bus-1600))
		(make-op :action 'take-bus-1700
			:add-list '(bus-1700))
		(make-op :action 'take-bus-1800
			:add-list '(bus-1800))
		(make-op :action 'take-bus-2000
			:add-list '(bus-2000))
		(make-op :action 'take-bus-2250
			:preconds '(bus-4250)
			:add-list '(bus-2250))
		(make-op :action 'take-bus-3200
			:preconds '(engl-2010)
			:add-list '(bus-3200))
		(make-op :action 'take-bus-3400
			:preconds '(acct-2010 math-1050 stat-1040 stat-1045 stat-2000 stat-2300 stat-3000 psy-3010)
			:add-list '(bus-3400))
		(make-op :action 'take-bus-4250
			:add-list '(bus-4250))
		(make-op :action 'take-chem-1010
			:add-list '(chem-1010))
		(make-op :action 'take-chem-1015
			:add-list '(chem-1015))
		(make-op :action 'take-chem-1110
			:preconds '(chem-1210 chem-1220 math-1050)
			:add-list '(chem-1110))
		(make-op :action 'take-chem-1115
			:add-list '(chem-1115))
		(make-op :action 'take-chem-1120
			:preconds '(chem-1110 chem-1110)
			:add-list '(chem-1120))
		(make-op :action 'take-chem-1125
			:add-list '(chem-1125))
		(make-op :action 'take-chem-1210
			:preconds '(math-1050)
			:add-list '(chem-1210))
		(make-op :action 'take-chem-1215
			:preconds '(chem-1210 chem-1210)
			:add-list '(chem-1215))
		(make-op :action 'take-chem-1220
			:preconds '(chem-1210 chem-1210)
			:add-list '(chem-1220))
		(make-op :action 'take-chem-1225
			:preconds '(chem-1215 chem-1220 chem-1215 chem-1220)
			:add-list '(chem-1225))
		(make-op :action 'take-chem-1990
			:add-list '(chem-1990))
		(make-op :action 'take-chem-2300
			:preconds '(chem-1210)
			:add-list '(chem-2300))
		(make-op :action 'take-chem-2310
			:preconds '(chem-1220 chem-1120)
			:add-list '(chem-2310))
		(make-op :action 'take-chem-2315
			:preconds '(chem-2310 chem-1215 chem-2300 chem-2310)
			:add-list '(chem-2315))
		(make-op :action 'take-chem-2320
			:preconds '(chem-2310 chem-2310 chem-2300)
			:add-list '(chem-2320))
		(make-op :action 'take-chem-2325
			:preconds '(chem-2315 chem-2315 chem-2320)
			:add-list '(chem-2325))
		(make-op :action 'take-chem-2977
			:add-list '(chem-2977))
		(make-op :action 'take-chem-2988
			:add-list '(chem-2988))
		(make-op :action 'take-chem-3000
			:preconds '(chem-1210 chem-1220 math-1050)
			:add-list '(chem-3000))
		(make-op :action 'take-chem-3005
			:preconds '(chem-1215 chem-1225 chem-3000 math-1050)
			:add-list '(chem-3005))
		(make-op :action 'take-chem-3060
			:preconds '(chem-1220 math-2210 phys-2220)
			:add-list '(chem-3060))
		(make-op :action 'take-chem-3070
			:preconds '(chem-3060)
			:add-list '(chem-3070))
		(make-op :action 'take-chem-3090
			:preconds '(chem-3060 chem-3070 chem-3060 chem-3070)
			:add-list '(chem-3090))
		(make-op :action 'take-chem-3510
			:preconds '(chem-1220 chem-2310 chem-2315)
			:add-list '(chem-3510))
		(make-op :action 'take-chem-3520
			:preconds '(chem-3510)
			:add-list '(chem-3520))
		(make-op :action 'take-chem-3650
			:preconds '(chem-1010 chem-1120 chem-1220)
			:add-list '(chem-3650))
		(make-op :action 'take-chem-3700
			:preconds '(chem-2300 chem-2310)
			:add-list '(chem-3700))
		(make-op :action 'take-chem-3710
			:preconds '(chem-3700 chem-3700)
			:add-list '(chem-3710))
		(make-op :action 'take-chem-3750
			:add-list '(chem-3750))
		(make-op :action 'take-chem-4250
			:add-list '(chem-4250))
		(make-op :action 'take-chem-4800
			:add-list '(chem-4800))
		(make-op :action 'take-chem-4890
			:preconds '(chem-5700 chem-5710)
			:add-list '(chem-4890))
		(make-op :action 'take-chem-4990
			:preconds '(chem-3060)
			:add-list '(chem-4990))
		(make-op :action 'take-chem-5070
			:preconds '(chem-1220 math-1220 phys-2120 phys-2220 chem-5700)
			:add-list '(chem-5070))
		(make-op :action 'take-chem-5100
			:preconds '(chem-3070)
			:add-list '(chem-5100))
		(make-op :action 'take-chem-5520
			:preconds '(chem-3070 chem-3510)
			:add-list '(chem-5520))
		(make-op :action 'take-chem-5530
			:preconds '(chem-2325 chem-3070 chem-3520)
			:add-list '(chem-5530))
		(make-op :action 'take-chem-5640
			:preconds '(chem-3000 chem-3060 chem-3070)
			:add-list '(chem-5640))
		(make-op :action 'take-chem-5650
			:preconds '(chem-5640 chem-3005 chem-3070 chem-3090)
			:add-list '(chem-5650))
		(make-op :action 'take-chem-5670
			:preconds '(chem-3000 chem-3005 chem-3070 chem-6670)
			:add-list '(chem-5670))
		(make-op :action 'take-chem-5680
			:preconds '(chem-5670 chem-3000 chem-3005 chem-5670)
			:add-list '(chem-5680))
		(make-op :action 'take-chem-5700
			:preconds '(chem-2320)
			:add-list '(chem-5700))
		(make-op :action 'take-chem-5710
			:preconds '(chem-5700 chem-5700)
			:add-list '(chem-5710))
		(make-op :action 'take-chem-5720
			:preconds '(engl-2010 chem-5710)
			:add-list '(chem-5720))
		(make-op :action 'take-chem-5790
			:preconds '(beng-5790 biol-5790 chem-6790)
			:add-list '(chem-5790))
		(make-op :action 'take-chin-1010
			:add-list '(chin-1010))
		(make-op :action 'take-chin-1020
			:preconds '(chin-1010)
			:add-list '(chin-1020))
		(make-op :action 'take-chin-1080
			:preconds '(chin-1010)
			:add-list '(chin-1080))
		(make-op :action 'take-chin-1800
			:add-list '(chin-1800))
		(make-op :action 'take-chin-2010
			:preconds '(chin-1020)
			:add-list '(chin-2010))
		(make-op :action 'take-chin-2020
			:preconds '(chin-2010)
			:add-list '(chin-2020))
		(make-op :action 'take-chin-2050
			:preconds '(chin-2010)
			:add-list '(chin-2050))
		(make-op :action 'take-chin-2800
			:add-list '(chin-2800))
		(make-op :action 'take-chin-3010
			:preconds '(chin-2020)
			:add-list '(chin-3010))
		(make-op :action 'take-chin-3020
			:preconds '(chin-3010)
			:add-list '(chin-3020))
		(make-op :action 'take-chin-3060
			:preconds '(chin-2020)
			:add-list '(chin-3060))
		(make-op :action 'take-chin-3080
			:add-list '(chin-3080))
		(make-op :action 'take-chin-3090
			:preconds '(chin-2020)
			:add-list '(chin-3090))
		(make-op :action 'take-chin-3100
			:preconds '(chin-2020)
			:add-list '(chin-3100))
		(make-op :action 'take-chin-3116
			:add-list '(chin-3116))
		(make-op :action 'take-chin-3117
			:add-list '(chin-3117))
		(make-op :action 'take-chin-3118
			:add-list '(chin-3118))
		(make-op :action 'take-chin-3510
			:preconds '(chin-2020)
			:add-list '(chin-3510))
		(make-op :action 'take-chin-3520
			:preconds '(chin-2020)
			:add-list '(chin-3520))
		(make-op :action 'take-chin-3540
			:preconds '(chin-2020)
			:add-list '(chin-3540))
		(make-op :action 'take-chin-3600
			:preconds '(chin-3010)
			:add-list '(chin-3600))
		(make-op :action 'take-chin-3800
			:add-list '(chin-3800))
		(make-op :action 'take-chin-3880
			:preconds '(chin-3020)
			:add-list '(chin-3880))
		(make-op :action 'take-chin-3900
			:preconds '(chin-2020)
			:add-list '(chin-3900))
		(make-op :action 'take-chin-4010
			:preconds '(chin-2020)
			:add-list '(chin-4010))
		(make-op :action 'take-chin-4090
			:add-list '(chin-4090))
		(make-op :action 'take-chin-4100
			:preconds '(chin-3010 chin-6100)
			:add-list '(chin-4100))
		(make-op :action 'take-chin-4210
			:preconds '(chin-2020)
			:add-list '(chin-4210))
		(make-op :action 'take-chin-4800
			:add-list '(chin-4800))
		(make-op :action 'take-chin-4920
			:add-list '(chin-4920))
		(make-op :action 'take-cee-1400
			:preconds '(math-1050)
			:add-list '(cee-1400))
		(make-op :action 'take-cee-1880
			:add-list '(cee-1880))
		(make-op :action 'take-cee-2240
			:preconds '(math-1050 math-1060)
			:add-list '(cee-2240))
		(make-op :action 'take-cee-2270
			:add-list '(cee-2270))
		(make-op :action 'take-cee-2450
			:preconds '(cee-1400 cs-1400 math-1210 math-1220 math-2250 math-2280)
			:add-list '(cee-2450))
		(make-op :action 'take-cee-2620
			:preconds '(biol-1010 chem-1220)
			:add-list '(cee-2620))
		(make-op :action 'take-cee-2890
			:add-list '(cee-2890))
		(make-op :action 'take-cee-3020
			:add-list '(cee-3020))
		(make-op :action 'take-cee-3080
			:preconds '(cee-3160 cee-3020)
			:add-list '(cee-3080))
		(make-op :action 'take-cee-3160
			:preconds '(engr-2140)
			:add-list '(cee-3160))
		(make-op :action 'take-cee-3165
			:preconds '(engr-2140 cee-3160)
			:add-list '(cee-3165))
		(make-op :action 'take-cee-3210
			:add-list '(cee-3210))
		(make-op :action 'take-cee-3430
			:preconds '(cee-3500)
			:add-list '(cee-3430))
		(make-op :action 'take-cee-3500
			:preconds '(math-1220 math-2210 math-2250 engr-2010 engr-2030)
			:add-list '(cee-3500))
		(make-op :action 'take-cee-3510
			:preconds '(cee-3500)
			:add-list '(cee-3510))
		(make-op :action 'take-cee-3610
			:preconds '(chem-1210 math-1210 pubh-3610)
			:add-list '(cee-3610))
		(make-op :action 'take-cee-3640
			:preconds '(cee-3610 pubh-3610)
			:add-list '(cee-3640))
		(make-op :action 'take-cee-3650
			:preconds '(cee-3610 pubh-3610)
			:add-list '(cee-3650))
		(make-op :action 'take-cee-3670
			:preconds '(beng-2400 mae-2300)
			:add-list '(cee-3670))
		(make-op :action 'take-cee-3780
			:add-list '(cee-3780))
		(make-op :action 'take-cee-3880
			:preconds '(engr-3080)
			:add-list '(cee-3880))
		(make-op :action 'take-cee-3890
			:preconds '(cee-3610 pubh-3610 cee-3640 cee-3670 beng-3670)
			:add-list '(cee-3890))
		(make-op :action 'take-cee-4200
			:add-list '(cee-4200))
		(make-op :action 'take-cee-4300
			:preconds '(engr-2140)
			:add-list '(cee-4300))
		(make-op :action 'take-cee-4790
			:preconds '(cee-3890 cee-3890)
			:add-list '(cee-4790))
		(make-op :action 'take-cee-4870
			:preconds '(cee-3880)
			:add-list '(cee-4870))
		(make-op :action 'take-cee-4880
			:preconds '(cee-4870)
			:add-list '(cee-4880))
		(make-op :action 'take-cee-4890
			:preconds '(cee-4790 cee-4790)
			:add-list '(cee-4890))
		(make-op :action 'take-cee-5000
			:preconds '(cee-3430 cee-3500 cee-6000)
			:add-list '(cee-5000))
		(make-op :action 'take-cee-5001
			:preconds '(cee-3430 cee-3500 psc-4700 cee-6001)
			:add-list '(cee-5001))
		(make-op :action 'take-cee-5003
			:preconds '(cee-2450 psc-5003 cee-6003)
			:add-list '(cee-5003))
		(make-op :action 'take-cee-5005
			:preconds '(cee-3430 cee-3500 psc-4700 cee-6005)
			:add-list '(cee-5005))
		(make-op :action 'take-cee-5006
			:preconds '(cee-5001 psc-4700 cee-6006)
			:add-list '(cee-5006))
		(make-op :action 'take-cee-5007
			:preconds '(cee-6007)
			:add-list '(cee-5007))
		(make-op :action 'take-cee-5008
			:preconds '(cee-3510 cee-5001 psc-4700 cee-6008)
			:add-list '(cee-5008))
		(make-op :action 'take-cee-5009
			:preconds '(cee-5000 cee-6000 cee-6009)
			:add-list '(cee-5009))
		(make-op :action 'take-cee-5010
			:preconds '(cee-3020)
			:add-list '(cee-5010))
		(make-op :action 'take-cee-5015
			:preconds '(cee-6015)
			:add-list '(cee-5015))
		(make-op :action 'take-cee-5020
			:preconds '(mae-3040 mae-5020)
			:add-list '(cee-5020))
		(make-op :action 'take-cee-5050
			:preconds '(cee-3020)
			:add-list '(cee-5050))
		(make-op :action 'take-cee-5060
			:preconds '(cee-3020 mae-3040 mae-5060)
			:add-list '(cee-5060))
		(make-op :action 'take-cee-5070
			:preconds '(cee-3020 cee-3080)
			:add-list '(cee-5070))
		(make-op :action 'take-cee-5080
			:preconds '(cee-3020 cee-6080)
			:add-list '(cee-5080))
		(make-op :action 'take-cee-5100
			:preconds '(cee-3080 cee-5070)
			:add-list '(cee-5100))
		(make-op :action 'take-cee-5190
			:preconds '(cee-6190)
			:add-list '(cee-5190))
		(make-op :action 'take-cee-5220
			:preconds '(cee-3210 cee-6220)
			:add-list '(cee-5220))
		(make-op :action 'take-cee-5230
			:preconds '(cee-3210 cee-6230)
			:add-list '(cee-5230))
		(make-op :action 'take-cee-5240
			:preconds '(cee-3210 cee-6240)
			:add-list '(cee-5240))
		(make-op :action 'take-cee-5250
			:add-list '(cee-5250))
		(make-op :action 'take-cee-5255
			:preconds '(cee-3210 cee-6255)
			:add-list '(cee-5255))
		(make-op :action 'take-cee-5350
			:preconds '(cee-4300 cee-6350)
			:add-list '(cee-5350))
		(make-op :action 'take-cee-5380
			:preconds '(cee-4300 cee-6380)
			:add-list '(cee-5380))
		(make-op :action 'take-cee-5410
			:preconds '(cee-6410)
			:add-list '(cee-5410))
		(make-op :action 'take-cee-5430
			:preconds '(cee-3430 cee-6430)
			:add-list '(cee-5430))
		(make-op :action 'take-cee-5450
			:preconds '(cee-3430 cee-6450)
			:add-list '(cee-5450))
		(make-op :action 'take-cee-5470
			:preconds '(cee-3500 cee-6470)
			:add-list '(cee-5470))
		(make-op :action 'take-cee-5490
			:preconds '(cee-6490)
			:add-list '(cee-5490))
		(make-op :action 'take-cee-5500
			:preconds '(cee-3500 cee-3510 cee-6500)
			:add-list '(cee-5500))
		(make-op :action 'take-cee-5540
			:preconds '(cee-3500 cee-3510 cee-6540)
			:add-list '(cee-5540))
		(make-op :action 'take-cee-5550
			:preconds '(cee-3500 cee-3510 cee-6550)
			:add-list '(cee-5550))
		(make-op :action 'take-cee-5600
			:preconds '(cee-6600)
			:add-list '(cee-5600))
		(make-op :action 'take-cee-5610
			:preconds '(chem-1210)
			:add-list '(cee-5610))
		(make-op :action 'take-cee-5620
			:preconds '(chem-1210 psc-5620)
			:add-list '(cee-5620))
		(make-op :action 'take-cee-5635
			:preconds '(cee-6635)
			:add-list '(cee-5635))
		(make-op :action 'take-cee-5655
			:preconds '(cee-6655)
			:add-list '(cee-5655))
		(make-op :action 'take-cee-5670
			:preconds '(chem-1210 pubh-5670)
			:add-list '(cee-5670))
		(make-op :action 'take-cee-5680
			:preconds '(beng-5680 cee-6680)
			:add-list '(cee-5680))
		(make-op :action 'take-cee-5690
			:preconds '(cee-3610 pubh-3610 cee-3500 cee-3510 cee-3640 cee-6690)
			:add-list '(cee-5690))
		(make-op :action 'take-cee-5710
			:add-list '(cee-5710))
		(make-op :action 'take-cee-5720
			:preconds '(cee-5690 cee-6690 cee-6720)
			:add-list '(cee-5720))
		(make-op :action 'take-cee-5730
			:preconds '(chem-1210 chem-1215 pubh-5730 cee-6730)
			:add-list '(cee-5730))
		(make-op :action 'take-cee-5750
			:add-list '(cee-5750))
		(make-op :action 'take-cee-5790
			:preconds '(chem-1220 pubh-5790)
			:add-list '(cee-5790))
		(make-op :action 'take-cee-5810
			:preconds '(beng-3200 beng-3670 cee-3670 beng-3670 cee-3670 cee-3610 pubh-3610 cee-3640 beng-5810 cee-6810)
			:add-list '(cee-5810))
		(make-op :action 'take-cee-5830
			:preconds '(beng-3670 cee-3670 beng-5830 cee-6830)
			:add-list '(cee-5830))
		(make-op :action 'take-cee-5860
			:preconds '(cee-3610)
			:add-list '(cee-5860))
		(make-op :action 'take-cee-5870
			:preconds '(cee-3780)
			:add-list '(cee-5870))
		(make-op :action 'take-cee-5880
			:preconds '(cee-3780 cee-3610 pubh-3610)
			:add-list '(cee-5880))
		(make-op :action 'take-cee-5885
			:preconds '(cee-3080 cee-6885)
			:add-list '(cee-5885))
		(make-op :action 'take-cee-5900
			:add-list '(cee-5900))
		(make-op :action 'take-cee-5930
			:add-list '(cee-5930))
		(make-op :action 'take-clas-1010
			:add-list '(clas-1010))
		(make-op :action 'take-clas-1120
			:preconds '(hist-1120)
			:add-list '(clas-1120))
		(make-op :action 'take-clas-1200
			:add-list '(clas-1200))
		(make-op :action 'take-clas-3160
			:preconds '(hist-3160)
			:add-list '(clas-3160))
		(make-op :action 'take-clas-3170
			:preconds '(hist-3170 rels-3170)
			:add-list '(clas-3170))
		(make-op :action 'take-clas-3210
			:preconds '(arth-3210 engl-3210 hist-3210 rels-3210)
			:add-list '(clas-3210))
		(make-op :action 'take-clas-3290
			:preconds '(hist-3290 rels-3290)
			:add-list '(clas-3290))
		(make-op :action 'take-tecd-1100
			:add-list '(tecd-1100))
		(make-op :action 'take-tecd-1150
			:add-list '(tecd-1150))
		(make-op :action 'take-tecd-1210
			:preconds '(tecd-1100)
			:add-list '(tecd-1210))
		(make-op :action 'take-tecd-1220
			:preconds '(tecd-1100)
			:add-list '(tecd-1220))
		(make-op :action 'take-cmst-1010
			:add-list '(cmst-1010))
		(make-op :action 'take-cmst-1020
			:add-list '(cmst-1020))
		(make-op :action 'take-cmst-1330
			:add-list '(cmst-1330))
		(make-op :action 'take-cmst-2110
			:add-list '(cmst-2110))
		(make-op :action 'take-cmst-2120
			:add-list '(cmst-2120))
		(make-op :action 'take-cmst-2250
			:add-list '(cmst-2250))
		(make-op :action 'take-cmst-2270
			:add-list '(cmst-2270))
		(make-op :action 'take-cmst-3000
			:add-list '(cmst-3000))
		(make-op :action 'take-cmst-3010
			:add-list '(cmst-3010))
		(make-op :action 'take-cmst-3020
			:preconds '(cmst-1020)
			:add-list '(cmst-3020))
		(make-op :action 'take-cmst-3050
			:add-list '(cmst-3050))
		(make-op :action 'take-cmst-3120
			:preconds '(cmst-2110)
			:add-list '(cmst-3120))
		(make-op :action 'take-cmst-3160
			:add-list '(cmst-3160))
		(make-op :action 'take-cmst-3250
			:preconds '(cmst-2110)
			:add-list '(cmst-3250))
		(make-op :action 'take-cmst-3270
			:add-list '(cmst-3270))
		(make-op :action 'take-cmst-3280
			:add-list '(cmst-3280))
		(make-op :action 'take-cmst-3300
			:preconds '(cmst-5370)
			:add-list '(cmst-3300))
		(make-op :action 'take-cmst-3330
			:add-list '(cmst-3330))
		(make-op :action 'take-cmst-3400
			:add-list '(cmst-3400))
		(make-op :action 'take-cmst-3510
			:add-list '(cmst-3510))
		(make-op :action 'take-cmst-3600
			:add-list '(cmst-3600))
		(make-op :action 'take-cmst-3610
			:add-list '(cmst-3610))
		(make-op :action 'take-cmst-3730
			:preconds '(cmst-1020 cmst-1330 cmst-3330)
			:add-list '(cmst-3730))
		(make-op :action 'take-cmst-4200
			:add-list '(cmst-4200))
		(make-op :action 'take-cmst-4250
			:add-list '(cmst-4250))
		(make-op :action 'take-cmst-4270
			:add-list '(cmst-4270))
		(make-op :action 'take-cmst-4300
			:preconds '(cmst-5370)
			:add-list '(cmst-4300))
		(make-op :action 'take-cmst-4330
			:preconds '(cmst-1330)
			:add-list '(cmst-4330))
		(make-op :action 'take-cmst-4430
			:add-list '(cmst-4430))
		(make-op :action 'take-cmst-4700
			:preconds '(cmst-2110)
			:add-list '(cmst-4700))
		(make-op :action 'take-cmst-4750
			:add-list '(cmst-4750))
		(make-op :action 'take-cmst-4760
			:add-list '(cmst-4760))
		(make-op :action 'take-cmst-4800
			:add-list '(cmst-4800))
		(make-op :action 'take-cmst-4810
			:preconds '(cmst-2110 stat-1040 stat-1045)
			:add-list '(cmst-4810))
		(make-op :action 'take-cmst-4820
			:preconds '(cmst-1020)
			:add-list '(cmst-4820))
		(make-op :action 'take-cmst-5000
			:add-list '(cmst-5000))
		(make-op :action 'take-cmst-5100
			:add-list '(cmst-5100))
		(make-op :action 'take-cmst-5110
			:preconds '(cmst-2110)
			:add-list '(cmst-5110))
		(make-op :action 'take-cmst-5250
			:add-list '(cmst-5250))
		(make-op :action 'take-cmst-5280
			:add-list '(cmst-5280))
		(make-op :action 'take-cmst-5300
			:add-list '(cmst-5300))
		(make-op :action 'take-cmst-5370
			:add-list '(cmst-5370))
		(make-op :action 'take-cmst-5400
			:preconds '(cmst-3400)
			:add-list '(cmst-5400))
		(make-op :action 'take-cmst-5500
			:preconds '(cmst-2120 cmst-3250)
			:add-list '(cmst-5500))
		(make-op :action 'take-cmst-5600
			:preconds '(cmst-3600)
			:add-list '(cmst-5600))
		(make-op :action 'take-cmst-5800
			:add-list '(cmst-5800))
		(make-op :action 'take-cmst-5950
			:preconds '(cmst-1330 cmst-3330)
			:add-list '(cmst-5950))
		(make-op :action 'take-comd-1030
			:add-list '(comd-1030))
		(make-op :action 'take-comd-1080
			:add-list '(comd-1080))
		(make-op :action 'take-comd-2400
			:add-list '(comd-2400))
		(make-op :action 'take-comd-2600
			:add-list '(comd-2600))
		(make-op :action 'take-comd-2800
			:add-list '(comd-2800))
		(make-op :action 'take-comd-2840
			:add-list '(comd-2840))
		(make-op :action 'take-comd-2860
			:add-list '(comd-2860))
		(make-op :action 'take-comd-3010
			:add-list '(comd-3010))
		(make-op :action 'take-comd-3050
			:add-list '(comd-3050))
		(make-op :action 'take-comd-3080
			:preconds '(comd-3010)
			:add-list '(comd-3080))
		(make-op :action 'take-comd-3085
			:preconds '(comd-3080 comd-3910)
			:add-list '(comd-3085))
		(make-op :action 'take-comd-3100
			:add-list '(comd-3100))
		(make-op :action 'take-comd-3120
			:preconds '(comd-3200 comd-3500)
			:add-list '(comd-3120))
		(make-op :action 'take-comd-3200
			:add-list '(comd-3200))
		(make-op :action 'take-comd-3400
			:add-list '(comd-3400))
		(make-op :action 'take-comd-3500
			:add-list '(comd-3500))
		(make-op :action 'take-comd-3600
			:add-list '(comd-3600))
		(make-op :action 'take-comd-3800
			:add-list '(comd-3800))
		(make-op :action 'take-comd-3850
			:add-list '(comd-3850))
		(make-op :action 'take-comd-3910
			:preconds '(comd-3010)
			:add-list '(comd-3910))
		(make-op :action 'take-comd-4100
			:add-list '(comd-4100))
		(make-op :action 'take-comd-4200
			:preconds '(comd-3200)
			:add-list '(comd-4200))
		(make-op :action 'take-comd-4300
			:preconds '(comd-3010 comd-3910 comd-4910 comd-4920 comd-4800)
			:add-list '(comd-4300))
		(make-op :action 'take-comd-4400
			:preconds '(comd-3400 comd-4700 comd-4200)
			:add-list '(comd-4400))
		(make-op :action 'take-comd-4450
			:preconds '(comd-3200)
			:add-list '(comd-4450))
		(make-op :action 'take-comd-4500
			:preconds '(comd-3100)
			:add-list '(comd-4500))
		(make-op :action 'take-comd-4600
			:add-list '(comd-4600))
		(make-op :action 'take-comd-4630
			:preconds '(comd-6630)
			:add-list '(comd-4630))
		(make-op :action 'take-comd-4700
			:preconds '(comd-3400 comd-3800)
			:add-list '(comd-4700))
		(make-op :action 'take-comd-4750
			:preconds '(comd-6750)
			:add-list '(comd-4750))
		(make-op :action 'take-comd-4760
			:preconds '(comd-6760)
			:add-list '(comd-4760))
		(make-op :action 'take-comd-4780
			:preconds '(comd-3010 comd-6780)
			:add-list '(comd-4780))
		(make-op :action 'take-comd-4800
			:preconds '(comd-3010 comd-3910 comd-4910 comd-4920 comd-4300)
			:add-list '(comd-4800))
		(make-op :action 'take-comd-4810
			:add-list '(comd-4810))
		(make-op :action 'take-comd-4820
			:add-list '(comd-4820))
		(make-op :action 'take-comd-4830
			:add-list '(comd-4830))
		(make-op :action 'take-comd-4850
			:add-list '(comd-4850))
		(make-op :action 'take-comd-4880
			:add-list '(comd-4880))
		(make-op :action 'take-comd-4890
			:add-list '(comd-4890))
		(make-op :action 'take-comd-4910
			:preconds '(comd-3910)
			:add-list '(comd-4910))
		(make-op :action 'take-comd-4920
			:preconds '(comd-4910)
			:add-list '(comd-4920))
		(make-op :action 'take-comd-5070
			:preconds '(comd-3400 comd-3800 comd-3500)
			:add-list '(comd-5070))
		(make-op :action 'take-comd-5200
			:preconds '(comd-3200 comd-3600)
			:add-list '(comd-5200))
		(make-op :action 'take-comd-5210
			:add-list '(comd-5210))
		(make-op :action 'take-comd-5240
			:preconds '(comd-3100)
			:add-list '(comd-5240))
		(make-op :action 'take-comd-5250
			:add-list '(comd-5250))
		(make-op :action 'take-comd-5330
			:preconds '(comd-3400)
			:add-list '(comd-5330))
		(make-op :action 'take-comd-5600
			:preconds '(comd-3010 comd-3910 comd-4910)
			:add-list '(comd-5600))
		(make-op :action 'take-comd-5610
			:preconds '(comd-3010)
			:add-list '(comd-5610))
		(make-op :action 'take-comd-5620
			:add-list '(comd-5620))
		(make-op :action 'take-comd-5630
			:preconds '(comd-3910)
			:add-list '(comd-5630))
		(make-op :action 'take-comd-5640
			:preconds '(comd-4810)
			:add-list '(comd-5640))
		(make-op :action 'take-comd-5720
			:preconds '(comd-4750 comd-4810)
			:add-list '(comd-5720))
		(make-op :action 'take-comd-5740
			:preconds '(comd-3910 comd-6740)
			:add-list '(comd-5740))
		(make-op :action 'take-comd-5750
			:add-list '(comd-5750))
		(make-op :action 'take-comd-5840
			:preconds '(comd-5850)
			:add-list '(comd-5840))
		(make-op :action 'take-comd-5850
			:preconds '(comd-5840)
			:add-list '(comd-5850))
		(make-op :action 'take-comd-5860
			:preconds '(comd-6860)
			:add-list '(comd-5860))
		(make-op :action 'take-comd-5870
			:preconds '(comd-6870)
			:add-list '(comd-5870))
		(make-op :action 'take-comd-5880
			:add-list '(comd-5880))
		(make-op :action 'take-comd-5900
			:add-list '(comd-5900))
		(make-op :action 'take-comd-5910
			:add-list '(comd-5910))
		(make-op :action 'take-comd-5920
			:add-list '(comd-5920))
		(make-op :action 'take-comd-5930
			:add-list '(comd-5930))
		(make-op :action 'take-comd-5950
			:add-list '(comd-5950))
		(make-op :action 'take-comd-5960
			:add-list '(comd-5960))
		(make-op :action 'take-cs-1030
			:add-list '(cs-1030))
		(make-op :action 'take-cs-1400
			:preconds '(math-1050)
			:add-list '(cs-1400))
		(make-op :action 'take-cs-1410
			:preconds '(cs-1400)
			:add-list '(cs-1410))
		(make-op :action 'take-cs-1440
			:preconds '(cs-1400 cs-1400)
			:add-list '(cs-1440))
		(make-op :action 'take-cs-2250
			:add-list '(cs-2250))
		(make-op :action 'take-cs-2410
			:preconds '(cs-1410)
			:add-list '(cs-2410))
		(make-op :action 'take-cs-2420
			:preconds '(cs-1410 ece-1410)
			:add-list '(cs-2420))
		(make-op :action 'take-cs-2610
			:preconds '(cs-1410)
			:add-list '(cs-2610))
		(make-op :action 'take-cs-2810
			:preconds '(cs-1410)
			:add-list '(cs-2810))
		(make-op :action 'take-cs-3010
			:add-list '(cs-3010))
		(make-op :action 'take-cs-3100
			:preconds '(cs-2420)
			:add-list '(cs-3100))
		(make-op :action 'take-cs-3200
			:preconds '(cs-2410)
			:add-list '(cs-3200))
		(make-op :action 'take-cs-3430
			:preconds '(cs-1410 math-1210 math-2270)
			:add-list '(cs-3430))
		(make-op :action 'take-cs-3450
			:preconds '(cs-2420 cs-2610)
			:add-list '(cs-3450))
		(make-op :action 'take-cs-3460
			:preconds '(cs-1440 cs-2420)
			:add-list '(cs-3460))
		(make-op :action 'take-cs-4250
			:add-list '(cs-4250))
		(make-op :action 'take-cs-4320
			:preconds '(cs-2420)
			:add-list '(cs-4320))
		(make-op :action 'take-cs-4350
			:preconds '(cs-1030)
			:add-list '(cs-4350))
		(make-op :action 'take-cs-4460
			:preconds '(cs-2420 cs-3100)
			:add-list '(cs-4460))
		(make-op :action 'take-cs-4610
			:preconds '(cs-2610)
			:add-list '(cs-4610))
		(make-op :action 'take-cs-4700
			:preconds '(cs-2420)
			:add-list '(cs-4700))
		(make-op :action 'take-cs-4890
			:preconds '(cs-2420)
			:add-list '(cs-4890))
		(make-op :action 'take-cs-4950
			:preconds '(cs-2420)
			:add-list '(cs-4950))
		(make-op :action 'take-cs-5000
			:preconds '(cs-2420)
			:add-list '(cs-5000))
		(make-op :action 'take-cs-5030
			:preconds '(cs-2420 cs-6030)
			:add-list '(cs-5030))
		(make-op :action 'take-cs-5040
			:preconds '(cs-2420 cs-6040)
			:add-list '(cs-5040))
		(make-op :action 'take-cs-5050
			:preconds '(cs-2420)
			:add-list '(cs-5050))
		(make-op :action 'take-cs-5060
			:preconds '(cs-2420)
			:add-list '(cs-5060))
		(make-op :action 'take-cs-5080
			:preconds '(cs-2420 cs-6080)
			:add-list '(cs-5080))
		(make-op :action 'take-cs-5110
			:preconds '(cs-2420 cs-6110)
			:add-list '(cs-5110))
		(make-op :action 'take-cs-5140
			:add-list '(cs-5140))
		(make-op :action 'take-cs-5250
			:preconds '(cs-2420 cs-5260)
			:add-list '(cs-5250))
		(make-op :action 'take-cs-5260
			:preconds '(cs-5250)
			:add-list '(cs-5260))
		(make-op :action 'take-cs-5300
			:preconds '(cs-2810)
			:add-list '(cs-5300))
		(make-op :action 'take-cs-5400
			:preconds '(cs-2420 math-2250 math-2270)
			:add-list '(cs-5400))
		(make-op :action 'take-cs-5410
			:preconds '(cs-2420 cs-3100)
			:add-list '(cs-5410))
		(make-op :action 'take-cs-5470
			:preconds '(cs-2420 math-2250 math-2270 cs-6470)
			:add-list '(cs-5470))
		(make-op :action 'take-cs-5510
			:preconds '(cs-2420 cs-6510)
			:add-list '(cs-5510))
		(make-op :action 'take-cs-5600
			:preconds '(cs-2420 cs-6600)
			:add-list '(cs-5600))
		(make-op :action 'take-cs-5620
			:preconds '(cs-2420 cs-6620)
			:add-list '(cs-5620))
		(make-op :action 'take-cs-5640
			:preconds '(cs-2420 cs-6640)
			:add-list '(cs-5640))
		(make-op :action 'take-cs-5665
			:preconds '(cs-2420)
			:add-list '(cs-5665))
		(make-op :action 'take-cs-5680
			:preconds '(cs-2420 math-2270 stat-2300 stat-3000 math-5710 cs-6680)
			:add-list '(cs-5680))
		(make-op :action 'take-cs-5700
			:preconds '(cs-3450)
			:add-list '(cs-5700))
		(make-op :action 'take-cs-5750
			:preconds '(stat-2300 stat-3000 cs-6750)
			:add-list '(cs-5750))
		(make-op :action 'take-cs-5800
			:preconds '(cs-2420)
			:add-list '(cs-5800))
		(make-op :action 'take-cs-5820
			:preconds '(cs-2420 cs-6820)
			:add-list '(cs-5820))
		(make-op :action 'take-cs-5830
			:preconds '(cs-2420 cs-6830)
			:add-list '(cs-5830))
		(make-op :action 'take-cs-5840
			:preconds '(cs-2420 math-2270 cs-5665 cs-6840)
			:add-list '(cs-5840))
		(make-op :action 'take-cs-5850
			:preconds '(cs-2420 cs-6850)
			:add-list '(cs-5850))
		(make-op :action 'take-cs-5890
			:preconds '(cs-2420)
			:add-list '(cs-5890))
		(make-op :action 'take-cs-5950
			:preconds '(cs-2420)
			:add-list '(cs-5950))
		(make-op :action 'take-tecs-1010
			:add-list '(tecs-1010))
		(make-op :action 'take-tecs-1020
			:add-list '(tecs-1020))
		(make-op :action 'take-tecs-1030
			:add-list '(tecs-1030))
		(make-op :action 'take-tecs-1040
			:add-list '(tecs-1040))
		(make-op :action 'take-tecs-1050
			:add-list '(tecs-1050))
		(make-op :action 'take-tecs-1060
			:add-list '(tecs-1060))
		(make-op :action 'take-tecs-1070
			:add-list '(tecs-1070))
		(make-op :action 'take-tecs-2310
			:add-list '(tecs-2310))
		(make-op :action 'take-tecs-2320
			:preconds '(tecs-2310)
			:add-list '(tecs-2320))
		(make-op :action 'take-tecs-2510
			:add-list '(tecs-2510))
		(make-op :action 'take-tecs-2830
			:add-list '(tecs-2830))
		(make-op :action 'take-tecs-2900
			:add-list '(tecs-2900))
		(make-op :action 'take-tecs-2910
			:preconds '(tecs-2900)
			:add-list '(tecs-2910))
		(make-op :action 'take-tecs-2920
			:preconds '(tecs-2910)
			:add-list '(tecs-2920))
		(make-op :action 'take-tecs-2930
			:preconds '(tecs-2920)
			:add-list '(tecs-2930))
		(make-op :action 'take-tecs-2941
			:add-list '(tecs-2941))
		(make-op :action 'take-tecs-2952
			:preconds '(tecs-2941)
			:add-list '(tecs-2952))
		(make-op :action 'take-cost-1000
			:add-list '(cost-1000))
		(make-op :action 'take-cost-1100
			:add-list '(cost-1100))
		(make-op :action 'take-cost-1110
			:add-list '(cost-1110))
		(make-op :action 'take-cost-1200
			:add-list '(cost-1200))
		(make-op :action 'take-cost-1210
			:add-list '(cost-1210))
		(make-op :action 'take-cost-1300
			:add-list '(cost-1300))
		(make-op :action 'take-cost-1310
			:add-list '(cost-1310))
		(make-op :action 'take-cost-1400
			:add-list '(cost-1400))
		(make-op :action 'take-cost-1407
			:add-list '(cost-1407))
		(make-op :action 'take-cost-1410
			:add-list '(cost-1410))
		(make-op :action 'take-cost-1500
			:add-list '(cost-1500))
		(make-op :action 'take-cost-1507
			:add-list '(cost-1507))
		(make-op :action 'take-cost-1602
			:add-list '(cost-1602))
		(make-op :action 'take-cost-2300
			:add-list '(cost-2300))
		(make-op :action 'take-cost-2310
			:add-list '(cost-2310))
		(make-op :action 'take-cost-2400
			:add-list '(cost-2400))
		(make-op :action 'take-cost-2410
			:add-list '(cost-2410))
		(make-op :action 'take-cost-2500
			:add-list '(cost-2500))
		(make-op :action 'take-cost-2510
			:add-list '(cost-2510))
		(make-op :action 'take-cost-2600
			:add-list '(cost-2600))
		(make-op :action 'take-cost-2700
			:add-list '(cost-2700))
		(make-op :action 'take-cost-2800
			:add-list '(cost-2800))
		(make-op :action 'take-cost-2977
			:add-list '(cost-2977))
		(make-op :action 'take-cost-2988
			:add-list '(cost-2988))
		(make-op :action 'take-cj-1010
			:add-list '(cj-1010))
		(make-op :action 'take-cj-1030
			:add-list '(cj-1030))
		(make-op :action 'take-cj-1300
			:add-list '(cj-1300))
		(make-op :action 'take-cj-1330
			:add-list '(cj-1330))
		(make-op :action 'take-cj-1340
			:add-list '(cj-1340))
		(make-op :action 'take-cj-1350
			:add-list '(cj-1350))
		(make-op :action 'take-cj-1355
			:add-list '(cj-1355))
		(make-op :action 'take-cj-1360
			:add-list '(cj-1360))
		(make-op :action 'take-cj-1390
			:add-list '(cj-1390))
		(make-op :action 'take-cj-1900
			:add-list '(cj-1900))
		(make-op :action 'take-cj-1901
			:add-list '(cj-1901))
		(make-op :action 'take-cj-1902
			:add-list '(cj-1902))
		(make-op :action 'take-cj-1903
			:add-list '(cj-1903))
		(make-op :action 'take-cj-1904
			:add-list '(cj-1904))
		(make-op :action 'take-cj-1905
			:add-list '(cj-1905))
		(make-op :action 'take-cj-1910
			:preconds '(cj-1900 cj-1901 cj-1902 cj-1903 cj-1904 cj-1905 cj-1912 cj-1913 cj-1914 cj-1915)
			:add-list '(cj-1910))
		(make-op :action 'take-cj-1912
			:preconds '(cj-1900 cj-1901 cj-1902 cj-1903 cj-1904 cj-1905)
			:add-list '(cj-1912))
		(make-op :action 'take-cj-1913
			:preconds '(cj-1900 cj-1901 cj-1902 cj-1903 cj-1904 cj-1905)
			:add-list '(cj-1913))
		(make-op :action 'take-cj-1914
			:preconds '(cj-1900 cj-1901 cj-1902 cj-1903 cj-1904 cj-1905)
			:add-list '(cj-1914))
		(make-op :action 'take-cj-1915
			:preconds '(cj-1900 cj-1901 cj-1902 cj-1903 cj-1904 cj-1905)
			:add-list '(cj-1915))
		(make-op :action 'take-cj-1920
			:preconds '(cj-1900 cj-1901 cj-1902 cj-1903 cj-1904 cj-1905 cj-1921 cj-1922 cj-1923 cj-1924 cj-1925 cj-1926)
			:add-list '(cj-1920))
		(make-op :action 'take-cj-1921
			:preconds '(cj-1900 cj-1901 cj-1902 cj-1903 cj-1904 cj-1905 cj-1920 cj-1922 cj-1923 cj-1924 cj-1925 cj-1926)
			:add-list '(cj-1921))
		(make-op :action 'take-cj-1922
			:preconds '(cj-1900 cj-1901 cj-1902 cj-1903 cj-1904 cj-1905)
			:add-list '(cj-1922))
		(make-op :action 'take-cj-1923
			:preconds '(cj-1900 cj-1901 cj-1902 cj-1903 cj-1904 cj-1905)
			:add-list '(cj-1923))
		(make-op :action 'take-cj-1924
			:preconds '(cj-1900 cj-1901 cj-1902 cj-1903 cj-1904 cj-1905)
			:add-list '(cj-1924))
		(make-op :action 'take-cj-1925
			:preconds '(cj-1900 cj-1901 cj-1902 cj-1903 cj-1904 cj-1905)
			:add-list '(cj-1925))
		(make-op :action 'take-cj-1926
			:preconds '(cj-1900 cj-1901 cj-1902 cj-1903 cj-1904 cj-1905)
			:add-list '(cj-1926))
		(make-op :action 'take-cj-2110
			:add-list '(cj-2110))
		(make-op :action 'take-cj-2330
			:add-list '(cj-2330))
		(make-op :action 'take-cj-2340
			:add-list '(cj-2340))
		(make-op :action 'take-cj-2350
			:add-list '(cj-2350))
		(make-op :action 'take-cj-2360
			:add-list '(cj-2360))
		(make-op :action 'take-cj-2370
			:add-list '(cj-2370))
		(make-op :action 'take-cj-2860
			:add-list '(cj-2860))
		(make-op :action 'take-cj-2977
			:add-list '(cj-2977))
		(make-op :action 'take-cj-2988
			:add-list '(cj-2988))
		(make-op :action 'take-teit-1040
			:add-list '(teit-1040))
		(make-op :action 'take-teit-1041
			:add-list '(teit-1041))
		(make-op :action 'take-teit-1050
			:add-list '(teit-1050))
		(make-op :action 'take-teit-1081
			:add-list '(teit-1081))
		(make-op :action 'take-teit-1091
			:add-list '(teit-1091))
		(make-op :action 'take-teit-1100
			:add-list '(teit-1100))
		(make-op :action 'take-teit-1110
			:add-list '(teit-1110))
		(make-op :action 'take-teit-1200
			:preconds '(teit-1210)
			:add-list '(teit-1200))
		(make-op :action 'take-teit-1210
			:preconds '(teit-1200)
			:add-list '(teit-1210))
		(make-op :action 'take-teit-1290
			:add-list '(teit-1290))
		(make-op :action 'take-teit-1300
			:add-list '(teit-1300))
		(make-op :action 'take-teit-1310
			:add-list '(teit-1310))
		(make-op :action 'take-teit-1340
			:add-list '(teit-1340))
		(make-op :action 'take-teit-1350
			:add-list '(teit-1350))
		(make-op :action 'take-teit-1400
			:add-list '(teit-1400))
		(make-op :action 'take-teit-1500
			:add-list '(teit-1500))
		(make-op :action 'take-teit-1550
			:add-list '(teit-1550))
		(make-op :action 'take-teit-1800
			:add-list '(teit-1800))
		(make-op :action 'take-teit-1810
			:add-list '(teit-1810))
		(make-op :action 'take-teit-1820
			:add-list '(teit-1820))
		(make-op :action 'take-teit-1830
			:add-list '(teit-1830))
		(make-op :action 'take-teit-1840
			:add-list '(teit-1840))
		(make-op :action 'take-teit-1850
			:add-list '(teit-1850))
		(make-op :action 'take-teit-1900
			:add-list '(teit-1900))
		(make-op :action 'take-teit-2100
			:add-list '(teit-2100))
		(make-op :action 'take-teit-2140
			:add-list '(teit-2140))
		(make-op :action 'take-teit-2160
			:add-list '(teit-2160))
		(make-op :action 'take-teit-2200
			:add-list '(teit-2200))
		(make-op :action 'take-teit-2250
			:add-list '(teit-2250))
		(make-op :action 'take-teit-2270
			:add-list '(teit-2270))
		(make-op :action 'take-teit-2300
			:add-list '(teit-2300))
		(make-op :action 'take-teit-2330
			:add-list '(teit-2330))
		(make-op :action 'take-teit-2441
			:add-list '(teit-2441))
		(make-op :action 'take-teit-2500
			:add-list '(teit-2500))
		(make-op :action 'take-teit-2610
			:add-list '(teit-2610))
		(make-op :action 'take-teit-2950
			:add-list '(teit-2950))
		(make-op :action 'take-teit-2977
			:add-list '(teit-2977))
		(make-op :action 'take-teit-2988
			:add-list '(teit-2988))
		(make-op :action 'take-danc-1010
			:add-list '(danc-1010))
		(make-op :action 'take-danc-1100
			:add-list '(danc-1100))
		(make-op :action 'take-danc-1200
			:add-list '(danc-1200))
		(make-op :action 'take-danc-1210
			:preconds '(danc-2310)
			:add-list '(danc-1210))
		(make-op :action 'take-danc-1220
			:add-list '(danc-1220))
		(make-op :action 'take-danc-1240
			:add-list '(danc-1240))
		(make-op :action 'take-danc-1500
			:add-list '(danc-1500))
		(make-op :action 'take-danc-1520
			:add-list '(danc-1520))
		(make-op :action 'take-danc-1580
			:add-list '(danc-1580))
		(make-op :action 'take-danc-2200
			:add-list '(danc-2200))
		(make-op :action 'take-danc-2210
			:add-list '(danc-2210))
		(make-op :action 'take-danc-2220
			:add-list '(danc-2220))
		(make-op :action 'take-danc-2240
			:add-list '(danc-2240))
		(make-op :action 'take-danc-2260
			:add-list '(danc-2260))
		(make-op :action 'take-danc-2310
			:add-list '(danc-2310))
		(make-op :action 'take-danc-2800
			:add-list '(danc-2800))
		(make-op :action 'take-danc-2977
			:add-list '(danc-2977))
		(make-op :action 'take-danc-2988
			:add-list '(danc-2988))
		(make-op :action 'take-danc-2999
			:add-list '(danc-2999))
		(make-op :action 'take-data-1100
			:preconds '(data-2100)
			:add-list '(data-1100))
		(make-op :action 'take-data-2100
			:preconds '(data-1100)
			:add-list '(data-2100))
		(make-op :action 'take-data-3100
			:preconds '(data-2100 data-2100 math-1050 math-1100)
			:add-list '(data-3100))
		(make-op :action 'take-data-3300
			:preconds '(data-3100 stat-1040 stat-1045 stat-2000 stat-2300 stat-3000)
			:add-list '(data-3300))
		(make-op :action 'take-data-3330
			:add-list '(data-3330))
		(make-op :action 'take-data-3400
			:add-list '(data-3400))
		(make-op :action 'take-data-3500
			:add-list '(data-3500))
		(make-op :action 'take-data-4330
			:preconds '(data-3330)
			:add-list '(data-4330))
		(make-op :action 'take-data-5330
			:preconds '(data-3330 data-3500 data-6330)
			:add-list '(data-5330))
		(make-op :action 'take-data-5350
			:preconds '(data-2100)
			:add-list '(data-5350))
		(make-op :action 'take-data-5360
			:preconds '(data-3330 data-6360)
			:add-list '(data-5360))
		(make-op :action 'take-data-5400
			:preconds '(data-6400)
			:add-list '(data-5400))
		(make-op :action 'take-data-5420
			:preconds '(data-3300 data-3500 cs-1400 data-3100 stat-1040 stat-1045 stat-2000 stat-2300 stat-3000 data-6420)
			:add-list '(data-5420))
		(make-op :action 'take-data-5480
			:preconds '(data-6480)
			:add-list '(data-5480))
		(make-op :action 'take-data-5500
			:preconds '(data-3500 data-6500)
			:add-list '(data-5500))
		(make-op :action 'take-data-5570
			:preconds '(data-3500 cs-1400 data-6570)
			:add-list '(data-5570))
		(make-op :action 'take-data-5600
			:preconds '(data-3100 data-3500)
			:add-list '(data-5600))
		(make-op :action 'take-data-5610
			:preconds '(data-5600 data-6610)
			:add-list '(data-5610))
		(make-op :action 'take-data-5630
			:preconds '(data-3500 cs-1400 data-5600 ecn-4330 stat-5100 data-6630)
			:add-list '(data-5630))
		(make-op :action 'take-data-5690
			:preconds '(data-3100 stat-3000 data-3500 cs-1400 fin-3200 cee-4200 data-5500 cs-2420 data-6690)
			:add-list '(data-5690))
		(make-op :action 'take-data-5695
			:preconds '(data-5690 data-6695)
			:add-list '(data-5695))
		(make-op :action 'take-dsme-1000
			:add-list '(dsme-1000))
		(make-op :action 'take-dsme-1110
			:add-list '(dsme-1110))
		(make-op :action 'take-dsme-1130
			:add-list '(dsme-1130))
		(make-op :action 'take-dsme-1310
			:add-list '(dsme-1310))
		(make-op :action 'take-dsme-1330
			:add-list '(dsme-1330))
		(make-op :action 'take-dsme-1340
			:add-list '(dsme-1340))
		(make-op :action 'take-dsme-1360
			:add-list '(dsme-1360))
		(make-op :action 'take-dsme-1500
			:add-list '(dsme-1500))
		(make-op :action 'take-dsme-2210
			:add-list '(dsme-2210))
		(make-op :action 'take-dsme-2230
			:add-list '(dsme-2230))
		(make-op :action 'take-dsme-2410
			:add-list '(dsme-2410))
		(make-op :action 'take-dsme-2430
			:add-list '(dsme-2430))
		(make-op :action 'take-dsme-2440
			:add-list '(dsme-2440))
		(make-op :action 'take-dsme-2460
			:add-list '(dsme-2460))
		(make-op :action 'take-dsme-2977
			:add-list '(dsme-2977))
		(make-op :action 'take-dsme-2988
			:add-list '(dsme-2988))
		(make-op :action 'take-ecn-1000
			:add-list '(ecn-1000))
		(make-op :action 'take-ecn-1500
			:add-list '(ecn-1500))
		(make-op :action 'take-ecn-2010
			:preconds '(ecn-1500 apec-1600)
			:add-list '(ecn-2010))
		(make-op :action 'take-ecn-3000
			:preconds '(math-1050)
			:add-list '(ecn-3000))
		(make-op :action 'take-ecn-3010
			:preconds '(ecn-2010 apec-2010 math-1100 stat-2300)
			:add-list '(ecn-3010))
		(make-op :action 'take-ecn-3170
			:preconds '(pols-3170)
			:add-list '(ecn-3170))
		(make-op :action 'take-ecn-3300
			:preconds '(msle-3300)
			:add-list '(ecn-3300))
		(make-op :action 'take-ecn-3400
			:preconds '(ecn-2010 apec-2010)
			:add-list '(ecn-3400))
		(make-op :action 'take-ecn-3500
			:add-list '(ecn-3500))
		(make-op :action 'take-ecn-3600
			:add-list '(ecn-3600))
		(make-op :action 'take-ecn-4010
			:preconds '(ecn-2010 apec-2010 math-1100 stat-2300 data-3100)
			:add-list '(ecn-4010))
		(make-op :action 'take-ecn-4020
			:preconds '(ecn-1500 ecn-2010 math-1100 math-1210)
			:add-list '(ecn-4020))
		(make-op :action 'take-ecn-4250
			:preconds '(ecn-1500 ecn-2010)
			:add-list '(ecn-4250))
		(make-op :action 'take-ecn-4310
			:preconds '(ecn-2010 apec-2010 math-1100 math-1210)
			:add-list '(ecn-4310))
		(make-op :action 'take-ecn-4330
			:preconds '(ecn-1500 ecn-2010 data-3100 stat-2000 stat-2300 stat-3000)
			:add-list '(ecn-4330))
		(make-op :action 'take-ecn-4500
			:add-list '(ecn-4500))
		(make-op :action 'take-ecn-4900
			:add-list '(ecn-4900))
		(make-op :action 'take-ecn-4950
			:add-list '(ecn-4950))
		(make-op :action 'take-ecn-5000
			:preconds '(ecn-4020)
			:add-list '(ecn-5000))
		(make-op :action 'take-ecn-5020
			:preconds '(ecn-4010 ecn-4020 ecn-6020)
			:add-list '(ecn-5020))
		(make-op :action 'take-ecn-5030
			:preconds '(ecn-4330)
			:add-list '(ecn-5030))
		(make-op :action 'take-ecn-5050
			:preconds '(ecn-2010)
			:add-list '(ecn-5050))
		(make-op :action 'take-ecn-5090
			:preconds '(ecn-6090)
			:add-list '(ecn-5090))
		(make-op :action 'take-ecn-5100
			:preconds '(ecn-2010 apec-2010)
			:add-list '(ecn-5100))
		(make-op :action 'take-ecn-5110
			:preconds '(ecn-2010 apec-2010)
			:add-list '(ecn-5110))
		(make-op :action 'take-ecn-5150
			:preconds '(ecn-2010 apec-2010)
			:add-list '(ecn-5150))
		(make-op :action 'take-ecn-5160
			:preconds '(ecn-4010 ecn-4020 ecn-4330)
			:add-list '(ecn-5160))
		(make-op :action 'take-ecn-5200
			:preconds '(ecn-4020)
			:add-list '(ecn-5200))
		(make-op :action 'take-ecn-5300
			:preconds '(ecn-4020 ecn-4010 apec-4010)
			:add-list '(ecn-5300))
		(make-op :action 'take-ecn-5320
			:preconds '(ecn-4310 ecn-6320)
			:add-list '(ecn-5320))
		(make-op :action 'take-ecn-5400
			:preconds '(ecn-4020 ecn-4010 apec-4010)
			:add-list '(ecn-5400))
		(make-op :action 'take-ecn-5500
			:preconds '(ecn-4010)
			:add-list '(ecn-5500))
		(make-op :action 'take-ecn-5700
			:preconds '(ecn-2010 apec-2010)
			:add-list '(ecn-5700))
		(make-op :action 'take-ecn-5800
			:preconds '(ecn-4020 ecn-4010 apec-4010)
			:add-list '(ecn-5800))
		(make-op :action 'take-ecn-5900
			:preconds '(ecn-4330)
			:add-list '(ecn-5900))
		(make-op :action 'take-ecn-5950
			:add-list '(ecn-5950))
		(make-op :action 'take-educ-100
			:add-list '(educ-100))
		(make-op :action 'take-educ-1000
			:add-list '(educ-1000))
		(make-op :action 'take-educ-1005
			:add-list '(educ-1005))
		(make-op :action 'take-educ-1020
			:add-list '(educ-1020))
		(make-op :action 'take-educ-1030
			:add-list '(educ-1030))
		(make-op :action 'take-educ-1040
			:add-list '(educ-1040))
		(make-op :action 'take-educ-1050
			:add-list '(educ-1050))
		(make-op :action 'take-educ-1800
			:add-list '(educ-1800))
		(make-op :action 'take-educ-2100
			:add-list '(educ-2100))
		(make-op :action 'take-educ-2110
			:add-list '(educ-2110))
		(make-op :action 'take-educ-2120
			:add-list '(educ-2120))
		(make-op :action 'take-educ-2977
			:add-list '(educ-2977))
		(make-op :action 'take-educ-2988
			:add-list '(educ-2988))
		(make-op :action 'take-educ-5000
			:add-list '(educ-5000))
		(make-op :action 'take-educ-5560
			:preconds '(teal-5560 educ-6560)
			:add-list '(educ-5560))
		(make-op :action 'take-teel-1110
			:add-list '(teel-1110))
		(make-op :action 'take-teel-1120
			:add-list '(teel-1120))
		(make-op :action 'take-teel-1210
			:preconds '(teel-1110)
			:add-list '(teel-1210))
		(make-op :action 'take-teel-1220
			:preconds '(teel-1110)
			:add-list '(teel-1220))
		(make-op :action 'take-teel-1320
			:add-list '(teel-1320))
		(make-op :action 'take-teel-1410
			:preconds '(teel-1110)
			:add-list '(teel-1410))
		(make-op :action 'take-teel-1420
			:preconds '(teel-1110)
			:add-list '(teel-1420))
		(make-op :action 'take-teit-1310
			:preconds '(teel-1110)
			:add-list '(teit-1310))
		(make-op :action 'take-ece-1400
			:preconds '(math-1050)
			:add-list '(ece-1400))
		(make-op :action 'take-ece-1410
			:preconds '(ece-1400)
			:add-list '(ece-1410))
		(make-op :action 'take-ece-2250
			:add-list '(ece-2250))
		(make-op :action 'take-ece-2290
			:preconds '(ece-2250 engr-2210 math-2250 math-2280)
			:add-list '(ece-2290))
		(make-op :action 'take-ece-2700
			:preconds '(ece-1400)
			:add-list '(ece-2700))
		(make-op :action 'take-ece-3410
			:preconds '(ece-2290 ece-3620)
			:add-list '(ece-3410))
		(make-op :action 'take-ece-3620
			:preconds '(ece-1400 ece-2290 engr-2210 math-2250 math-2280)
			:add-list '(ece-3620))
		(make-op :action 'take-ece-3640
			:preconds '(ece-3620)
			:add-list '(ece-3640))
		(make-op :action 'take-ece-3700
			:preconds '(ece-2700)
			:add-list '(ece-3700))
		(make-op :action 'take-ece-3710
			:preconds '(ece-1400 engr-2210 ece-2250 ece-2700)
			:add-list '(ece-3710))
		(make-op :action 'take-ece-3810
			:preconds '(engr-3080)
			:add-list '(ece-3810))
		(make-op :action 'take-ece-3870
			:preconds '(ece-2290 math-2210 phys-2220 math-2250 math-2270 math-2280)
			:add-list '(ece-3870))
		(make-op :action 'take-ece-4250
			:add-list '(ece-4250))
		(make-op :action 'take-ece-4820
			:preconds '(ece-3410 ece-3710 ece-3810 ece-4830)
			:add-list '(ece-4820))
		(make-op :action 'take-ece-4830
			:preconds '(ece-3410 ece-3710 ece-3810 ece-4820)
			:add-list '(ece-4830))
		(make-op :action 'take-ece-4840
			:preconds '(ece-4820 ece-4820 ece-4850)
			:add-list '(ece-4840))
		(make-op :action 'take-ece-4850
			:preconds '(ece-4830 ece-4840)
			:add-list '(ece-4850))
		(make-op :action 'take-ece-5110
			:preconds '(ece-2250)
			:add-list '(ece-5110))
		(make-op :action 'take-ece-5120
			:preconds '(ece-3410 ece-3620 ece-6120)
			:add-list '(ece-5120))
		(make-op :action 'take-ece-5150
			:preconds '(ece-3410 ece-3620 ece-3870 ece-6150)
			:add-list '(ece-5150))
		(make-op :action 'take-ece-5160
			:preconds '(ece-3710 ece-5120 ece-6120 ece-5150 ece-6150 ece-6160)
			:add-list '(ece-5160))
		(make-op :action 'take-ece-5210
			:preconds '(phys-3600 ece-3870 phys-5210)
			:add-list '(ece-5210))
		(make-op :action 'take-ece-5230
			:preconds '(ece-2290 engr-2210 math-2250 math-2270 math-2280)
			:add-list '(ece-5230))
		(make-op :action 'take-ece-5240
			:preconds '(ece-5230 mae-5520 mae-5530)
			:add-list '(ece-5240))
		(make-op :action 'take-ece-5250
			:preconds '(phys-4650 phys-4600 ece-5800 phys-5250)
			:add-list '(ece-5250))
		(make-op :action 'take-ece-5310
			:preconds '(ece-3620)
			:add-list '(ece-5310))
		(make-op :action 'take-ece-5320
			:preconds '(ece-5310 mae-5310 mae-5320)
			:add-list '(ece-5320))
		(make-op :action 'take-ece-5330
			:preconds '(ece-5310 mae-5310 mae-5330)
			:add-list '(ece-5330))
		(make-op :action 'take-ece-5340
			:preconds '(ece-1410 cs-1410 math-2250 math-2280 mae-5340)
			:add-list '(ece-5340))
		(make-op :action 'take-ece-5345
			:preconds '(ece-1410 cs-1410)
			:add-list '(ece-5345))
		(make-op :action 'take-ece-5420
			:preconds '(ece-3410)
			:add-list '(ece-5420))
		(make-op :action 'take-ece-5460
			:preconds '(ece-1400 ece-2700 ece-6460)
			:add-list '(ece-5460))
		(make-op :action 'take-ece-5470
			:preconds '(ece-5460 ece-6460 ece-6470)
			:add-list '(ece-5470))
		(make-op :action 'take-ece-5480
			:preconds '(ece-2700 ece-6480)
			:add-list '(ece-5480))
		(make-op :action 'take-ece-5600
			:preconds '(ece-3620 math-5710 stat-3000)
			:add-list '(ece-5600))
		(make-op :action 'take-ece-5630
			:preconds '(ece-3640)
			:add-list '(ece-5630))
		(make-op :action 'take-ece-5640
			:preconds '(ece-3640 ece-3710)
			:add-list '(ece-5640))
		(make-op :action 'take-ece-5660
			:preconds '(ece-3640 math-5710)
			:add-list '(ece-5660))
		(make-op :action 'take-ece-5700
			:preconds '(phys-2210 phys-2310 phys-2215 phys-2220 phys-2320 phys-2225 beng-5700 mae-5700 phys-5700)
			:add-list '(ece-5700))
		(make-op :action 'take-ece-5710
			:preconds '(beng-5700 ece-5700 mae-5700 phys-5700 beng-5710 phys-5710)
			:add-list '(ece-5710))
		(make-op :action 'take-ece-5720
			:preconds '(ece-2700 ece-3710)
			:add-list '(ece-5720))
		(make-op :action 'take-ece-5730
			:preconds '(ece-2700 ece-3710 ece-6730)
			:add-list '(ece-5730))
		(make-op :action 'take-ece-5750
			:preconds '(ece-5720 ece-6750)
			:add-list '(ece-5750))
		(make-op :action 'take-ece-5780
			:preconds '(ece-3710 ece-6780)
			:add-list '(ece-5780))
		(make-op :action 'take-ece-5790
			:preconds '(math-3310 cs-2420 ece-6790)
			:add-list '(ece-5790))
		(make-op :action 'take-ece-5800
			:preconds '(ece-3870)
			:add-list '(ece-5800))
		(make-op :action 'take-ece-5810
			:preconds '(ece-3870)
			:add-list '(ece-5810))
		(make-op :action 'take-ece-5850
			:preconds '(ece-3870)
			:add-list '(ece-5850))
		(make-op :action 'take-ece-5930
			:add-list '(ece-5930))
		(make-op :action 'take-elet-1110
			:add-list '(elet-1110))
		(make-op :action 'take-elet-1120
			:add-list '(elet-1120))
		(make-op :action 'take-elet-1130
			:add-list '(elet-1130))
		(make-op :action 'take-elet-1140
			:add-list '(elet-1140))
		(make-op :action 'take-elet-1150
			:preconds '(elet-1110)
			:add-list '(elet-1150))
		(make-op :action 'take-elet-1160
			:add-list '(elet-1160))
		(make-op :action 'take-elet-1170
			:add-list '(elet-1170))
		(make-op :action 'take-elet-1180
			:add-list '(elet-1180))
		(make-op :action 'take-elet-1200
			:add-list '(elet-1200))
		(make-op :action 'take-elet-1210
			:preconds '(elet-1200)
			:add-list '(elet-1210))
		(make-op :action 'take-elet-2110
			:add-list '(elet-2110))
		(make-op :action 'take-elet-2120
			:preconds '(elet-2110)
			:add-list '(elet-2120))
		(make-op :action 'take-elet-2130
			:preconds '(elet-1110)
			:add-list '(elet-2130))
		(make-op :action 'take-elet-2140
			:add-list '(elet-2140))
		(make-op :action 'take-elet-2150
			:preconds '(elet-1200)
			:add-list '(elet-2150))
		(make-op :action 'take-elet-2160
			:preconds '(elet-1200 elet-1210)
			:add-list '(elet-2160))
		(make-op :action 'take-elet-2210
			:add-list '(elet-2210))
		(make-op :action 'take-elet-2220
			:add-list '(elet-2220))
		(make-op :action 'take-elet-2230
			:add-list '(elet-2230))
		(make-op :action 'take-elet-2240
			:add-list '(elet-2240))
		(make-op :action 'take-elet-2250
			:add-list '(elet-2250))
		(make-op :action 'take-elet-2260
			:add-list '(elet-2260))
		(make-op :action 'take-elet-2270
			:add-list '(elet-2270))
		(make-op :action 'take-elet-2280
			:preconds '(elet-2270)
			:add-list '(elet-2280))
		(make-op :action 'take-elet-2330
			:add-list '(elet-2330))
		(make-op :action 'take-elet-2340
			:add-list '(elet-2340))
		(make-op :action 'take-elet-2510
			:add-list '(elet-2510))
		(make-op :action 'take-elet-2520
			:add-list '(elet-2520))
		(make-op :action 'take-elet-2977
			:add-list '(elet-2977))
		(make-op :action 'take-elet-2988
			:add-list '(elet-2988))
		(make-op :action 'take-eled-1010
			:add-list '(eled-1010))
		(make-op :action 'take-eled-2480
			:preconds '(eled-1010 hdfs-1500)
			:add-list '(eled-2480))
		(make-op :action 'take-eled-3000
			:preconds '(eled-3002 eled-3100)
			:add-list '(eled-3000))
		(make-op :action 'take-eled-3001
			:add-list '(eled-3001))
		(make-op :action 'take-eled-3002
			:preconds '(eled-3000 eled-3100)
			:add-list '(eled-3002))
		(make-op :action 'take-eled-3003
			:preconds '(eled-3001 eled-3101)
			:add-list '(eled-3003))
		(make-op :action 'take-eled-3010
			:add-list '(eled-3010))
		(make-op :action 'take-eled-3100
			:preconds '(eled-1010 teal-1010 hdfs-1500 eled-3000 eled-3001)
			:add-list '(eled-3100))
		(make-op :action 'take-eled-3101
			:preconds '(eled-3001 eled-3003)
			:add-list '(eled-3101))
		(make-op :action 'take-eled-3200
			:preconds '(eled-3100)
			:add-list '(eled-3200))
		(make-op :action 'take-eled-3800
			:add-list '(eled-3800))
		(make-op :action 'take-eled-4010
			:add-list '(eled-4010))
		(make-op :action 'take-eled-4020
			:preconds '(eled-4050 eled-4062 eled-4056)
			:add-list '(eled-4020))
		(make-op :action 'take-eled-4021
			:preconds '(eled-3000 eled-3001 eled-4031 eled-4051 eled-4063 eled-4065)
			:add-list '(eled-4021))
		(make-op :action 'take-eled-4030
			:preconds '(eled-3100 eled-4020 eled-4050 eled-4056 eled-4062)
			:add-list '(eled-4030))
		(make-op :action 'take-eled-4031
			:preconds '(eled-3000 eled-3002 eled-3100 eled-4021 eled-4051 eled-4063 eled-4065)
			:add-list '(eled-4031))
		(make-op :action 'take-eled-4040
			:preconds '(eled-3100 eled-4042)
			:add-list '(eled-4040))
		(make-op :action 'take-eled-4041
			:preconds '(eled-3000 eled-3002 eled-3100 eled-4043)
			:add-list '(eled-4041))
		(make-op :action 'take-eled-4042
			:preconds '(eled-4040)
			:add-list '(eled-4042))
		(make-op :action 'take-eled-4043
			:preconds '(eled-3100 eled-4041)
			:add-list '(eled-4043))
		(make-op :action 'take-eled-4050
			:preconds '(eled-4020 eled-4030 eled-4030 eled-4056 eled-4062)
			:add-list '(eled-4050))
		(make-op :action 'take-eled-4051
			:preconds '(eled-3000 eled-3001 eled-4021 eled-4031 eled-4063 eled-4065)
			:add-list '(eled-4051))
		(make-op :action 'take-eled-4056
			:preconds '(eled-4020 eled-4030 eled-4050 eled-4062)
			:add-list '(eled-4056))
		(make-op :action 'take-eled-4060
			:add-list '(eled-4060))
		(make-op :action 'take-eled-4061
			:preconds '(math-2010 math-2020)
			:add-list '(eled-4061))
		(make-op :action 'take-eled-4062
			:preconds '(eled-4061 sped-5340 eled-4020 eled-4030 eled-4050 eled-4056)
			:add-list '(eled-4062))
		(make-op :action 'take-eled-4063
			:preconds '(eled-4064 eled-4021 eled-4031 eled-4051 eled-4065)
			:add-list '(eled-4063))
		(make-op :action 'take-eled-4064
			:preconds '(math-1050 math-2010 math-2020)
			:add-list '(eled-4064))
		(make-op :action 'take-eled-4065
			:preconds '(eled-4021 eled-4031 eled-4051 eled-4063)
			:add-list '(eled-4065))
		(make-op :action 'take-eled-4150
			:preconds '(eled-3000 eled-3001)
			:add-list '(eled-4150))
		(make-op :action 'take-eled-4151
			:preconds '(eled-3000 eled-3001)
			:add-list '(eled-4151))
		(make-op :action 'take-eled-4250
			:add-list '(eled-4250))
		(make-op :action 'take-eled-4410
			:add-list '(eled-4410))
		(make-op :action 'take-eled-4900
			:add-list '(eled-4900))
		(make-op :action 'take-eled-4970
			:add-list '(eled-4970))
		(make-op :action 'take-eled-5050
			:preconds '(eled-5250)
			:add-list '(eled-5050))
		(make-op :action 'take-eled-5105
			:add-list '(eled-5105))
		(make-op :action 'take-eled-5150
			:add-list '(eled-5150))
		(make-op :action 'take-eled-5240
			:preconds '(eled-5250)
			:add-list '(eled-5240))
		(make-op :action 'take-eled-5250
			:preconds '(eled-5050 eled-5150 eled-5240)
			:add-list '(eled-5250))
		(make-op :action 'take-eled-5300
			:add-list '(eled-5300))
		(make-op :action 'take-eled-5900
			:add-list '(eled-5900))
		(make-op :action 'take-teem-2450
			:add-list '(teem-2450))
		(make-op :action 'take-engr-1000
			:add-list '(engr-1000))
		(make-op :action 'take-engr-1005
			:add-list '(engr-1005))
		(make-op :action 'take-engr-1500
			:add-list '(engr-1500))
		(make-op :action 'take-engr-1510
			:preconds '(math-1050 math-1060)
			:add-list '(engr-1510))
		(make-op :action 'take-engr-1940
			:add-list '(engr-1940))
		(make-op :action 'take-engr-2010
			:preconds '(math-1210 math-1220 phys-2210)
			:add-list '(engr-2010))
		(make-op :action 'take-engr-2030
			:preconds '(engr-2010 math-1210 math-1220)
			:add-list '(engr-2030))
		(make-op :action 'take-engr-2140
			:preconds '(engr-2010)
			:add-list '(engr-2140))
		(make-op :action 'take-engr-2210
			:preconds '(math-1210 math-1220)
			:add-list '(engr-2210))
		(make-op :action 'take-engr-2450
			:preconds '(math-1220 math-2250 math-2280 math-2250 math-2280)
			:add-list '(engr-2450))
		(make-op :action 'take-engr-2455
			:add-list '(engr-2455))
		(make-op :action 'take-engr-2930
			:add-list '(engr-2930))
		(make-op :action 'take-engr-2977
			:add-list '(engr-2977))
		(make-op :action 'take-engr-2999
			:add-list '(engr-2999))
		(make-op :action 'take-engr-3080
			:preconds '(engl-2010)
			:add-list '(engr-3080))
		(make-op :action 'take-engr-5930
			:add-list '(engr-5930))
		(make-op :action 'take-eddt-1010
			:add-list '(eddt-1010))
		(make-op :action 'take-eddt-1040
			:add-list '(eddt-1040))
		(make-op :action 'take-eddt-1070
			:add-list '(eddt-1070))
		(make-op :action 'take-eddt-1100
			:add-list '(eddt-1100))
		(make-op :action 'take-eddt-1200
			:add-list '(eddt-1200))
		(make-op :action 'take-eddt-1500
			:add-list '(eddt-1500))
		(make-op :action 'take-eddt-1700
			:add-list '(eddt-1700))
		(make-op :action 'take-eddt-2100
			:add-list '(eddt-2100))
		(make-op :action 'take-eddt-2500
			:add-list '(eddt-2500))
		(make-op :action 'take-eddt-2620
			:add-list '(eddt-2620))
		(make-op :action 'take-eddt-2650
			:add-list '(eddt-2650))
		(make-op :action 'take-eddt-2700
			:add-list '(eddt-2700))
		(make-op :action 'take-eddt-2977
			:add-list '(eddt-2977))
		(make-op :action 'take-eddt-2988
			:add-list '(eddt-2988))
		(make-op :action 'take-etec-1000
			:add-list '(etec-1000))
		(make-op :action 'take-etec-1010
			:add-list '(etec-1010))
		(make-op :action 'take-etec-2300
			:add-list '(etec-2300))
		(make-op :action 'take-engl-10
			:preconds '(engl-1010)
			:add-list '(engl-10))
		(make-op :action 'take-engl-1010
			:add-list '(engl-1010))
		(make-op :action 'take-engl-1020
			:add-list '(engl-1020))
		(make-op :action 'take-engl-1050
			:add-list '(engl-1050))
		(make-op :action 'take-engl-1055
			:add-list '(engl-1055))
		(make-op :action 'take-engl-1410
			:add-list '(engl-1410))
		(make-op :action 'take-engl-2010
			:preconds '(engl-1010)
			:add-list '(engl-2010))
		(make-op :action 'take-engl-2020
			:preconds '(engl-1010)
			:add-list '(engl-2020))
		(make-op :action 'take-engl-2030
			:add-list '(engl-2030))
		(make-op :action 'take-engl-2040
			:add-list '(engl-2040))
		(make-op :action 'take-engl-2050
			:add-list '(engl-2050))
		(make-op :action 'take-engl-2060
			:add-list '(engl-2060))
		(make-op :action 'take-engl-2070
			:add-list '(engl-2070))
		(make-op :action 'take-engl-2120
			:add-list '(engl-2120))
		(make-op :action 'take-engl-2140
			:add-list '(engl-2140))
		(make-op :action 'take-engl-2150
			:add-list '(engl-2150))
		(make-op :action 'take-engl-2200
			:add-list '(engl-2200))
		(make-op :action 'take-engl-2210
			:preconds '(anth-2210 hist-2210)
			:add-list '(engl-2210))
		(make-op :action 'take-engl-2220
			:add-list '(engl-2220))
		(make-op :action 'take-engl-2230
			:add-list '(engl-2230))
		(make-op :action 'take-engl-2240
			:add-list '(engl-2240))
		(make-op :action 'take-engl-2250
			:add-list '(engl-2250))
		(make-op :action 'take-engl-2260
			:add-list '(engl-2260))
		(make-op :action 'take-engl-2300
			:add-list '(engl-2300))
		(make-op :action 'take-engl-2330
			:add-list '(engl-2330))
		(make-op :action 'take-engl-2340
			:add-list '(engl-2340))
		(make-op :action 'take-engl-2350
			:add-list '(engl-2350))
		(make-op :action 'take-engl-2510
			:add-list '(engl-2510))
		(make-op :action 'take-engl-2520
			:add-list '(engl-2520))
		(make-op :action 'take-engl-2600
			:add-list '(engl-2600))
		(make-op :action 'take-engl-2610
			:add-list '(engl-2610))
		(make-op :action 'take-engl-2620
			:add-list '(engl-2620))
		(make-op :action 'take-engl-2630
			:add-list '(engl-2630))
		(make-op :action 'take-engl-2640
			:add-list '(engl-2640))
		(make-op :action 'take-engl-2720
			:preconds '(anth-2720 hist-2720)
			:add-list '(engl-2720))
		(make-op :action 'take-engl-2977
			:add-list '(engl-2977))
		(make-op :action 'take-engl-2988
			:add-list '(engl-2988))
		(make-op :action 'take-engl-3020
			:add-list '(engl-3020))
		(make-op :action 'take-engl-3030
			:add-list '(engl-3030))
		(make-op :action 'take-engl-3040
			:add-list '(engl-3040))
		(make-op :action 'take-engl-3050
			:add-list '(engl-3050))
		(make-op :action 'take-engl-3060
			:add-list '(engl-3060))
		(make-op :action 'take-engl-3070
			:preconds '(hist-3070)
			:add-list '(engl-3070))
		(make-op :action 'take-engl-3080
			:add-list '(engl-3080))
		(make-op :action 'take-engl-3085
			:add-list '(engl-3085))
		(make-op :action 'take-engl-3210
			:preconds '(arth-3210 clas-3210 hist-3210 rels-3210)
			:add-list '(engl-3210))
		(make-op :action 'take-engl-3300
			:add-list '(engl-3300))
		(make-op :action 'take-engl-3305
			:add-list '(engl-3305))
		(make-op :action 'take-engl-3310
			:add-list '(engl-3310))
		(make-op :action 'take-engl-3315
			:add-list '(engl-3315))
		(make-op :action 'take-engl-3320
			:add-list '(engl-3320))
		(make-op :action 'take-engl-3325
			:add-list '(engl-3325))
		(make-op :action 'take-engl-3335
			:add-list '(engl-3335))
		(make-op :action 'take-engl-3345
			:add-list '(engl-3345))
		(make-op :action 'take-engl-3355
			:add-list '(engl-3355))
		(make-op :action 'take-engl-3365
			:add-list '(engl-3365))
		(make-op :action 'take-engl-3375
			:add-list '(engl-3375))
		(make-op :action 'take-engl-3385
			:add-list '(engl-3385))
		(make-op :action 'take-engl-3395
			:add-list '(engl-3395))
		(make-op :action 'take-engl-3420
			:add-list '(engl-3420))
		(make-op :action 'take-engl-3430
			:add-list '(engl-3430))
		(make-op :action 'take-engl-3440
			:add-list '(engl-3440))
		(make-op :action 'take-engl-3470
			:add-list '(engl-3470))
		(make-op :action 'take-engl-3500
			:preconds '(sced-3300)
			:add-list '(engl-3500))
		(make-op :action 'take-engl-3510
			:add-list '(engl-3510))
		(make-op :action 'take-engl-3530
			:add-list '(engl-3530))
		(make-op :action 'take-engl-3610
			:add-list '(engl-3610))
		(make-op :action 'take-engl-3620
			:add-list '(engl-3620))
		(make-op :action 'take-engl-3630
			:add-list '(engl-3630))
		(make-op :action 'take-engl-3640
			:add-list '(engl-3640))
		(make-op :action 'take-engl-3700
			:preconds '(hist-3700)
			:add-list '(engl-3700))
		(make-op :action 'take-engl-3710
			:preconds '(anth-3710 hist-3710 rels-3710)
			:add-list '(engl-3710))
		(make-op :action 'take-engl-3720
			:add-list '(engl-3720))
		(make-op :action 'take-engl-3830
			:preconds '(hist-3830 kin-3830 rels-3830)
			:add-list '(engl-3830))
		(make-op :action 'take-engl-4200
			:add-list '(engl-4200))
		(make-op :action 'take-engl-4210
			:add-list '(engl-4210))
		(make-op :action 'take-engl-4220
			:add-list '(engl-4220))
		(make-op :action 'take-engl-4230
			:add-list '(engl-4230))
		(make-op :action 'take-engl-4250
			:preconds '(thea-1713 thea-4760)
			:add-list '(engl-4250))
		(make-op :action 'take-engl-4260
			:add-list '(engl-4260))
		(make-op :action 'take-engl-4300
			:add-list '(engl-4300))
		(make-op :action 'take-engl-4310
			:add-list '(engl-4310))
		(make-op :action 'take-engl-4320
			:add-list '(engl-4320))
		(make-op :action 'take-engl-4330
			:add-list '(engl-4330))
		(make-op :action 'take-engl-4340
			:add-list '(engl-4340))
		(make-op :action 'take-engl-4345
			:add-list '(engl-4345))
		(make-op :action 'take-engl-4350
			:add-list '(engl-4350))
		(make-op :action 'take-engl-4360
			:add-list '(engl-4360))
		(make-op :action 'take-engl-4365
			:add-list '(engl-4365))
		(make-op :action 'take-engl-4370
			:add-list '(engl-4370))
		(make-op :action 'take-engl-4375
			:add-list '(engl-4375))
		(make-op :action 'take-engl-4380
			:add-list '(engl-4380))
		(make-op :action 'take-engl-4420
			:preconds '(engl-3420 engl-3430 engl-3440)
			:add-list '(engl-4420))
		(make-op :action 'take-engl-4430
			:preconds '(engl-3430 engl-3420 engl-3440)
			:add-list '(engl-4430))
		(make-op :action 'take-engl-4440
			:preconds '(engl-3440 engl-3420 engl-3430)
			:add-list '(engl-4440))
		(make-op :action 'take-engl-4500
			:add-list '(engl-4500))
		(make-op :action 'take-engl-4510
			:add-list '(engl-4510))
		(make-op :action 'take-engl-4520
			:add-list '(engl-4520))
		(make-op :action 'take-engl-4530
			:add-list '(engl-4530))
		(make-op :action 'take-engl-4540
			:add-list '(engl-4540))
		(make-op :action 'take-engl-4610
			:add-list '(engl-4610))
		(make-op :action 'take-engl-4620
			:preconds '(hist-4620)
			:add-list '(engl-4620))
		(make-op :action 'take-engl-4630
			:add-list '(engl-4630))
		(make-op :action 'take-engl-4640
			:add-list '(engl-4640))
		(make-op :action 'take-engl-4650
			:preconds '(anth-4650)
			:add-list '(engl-4650))
		(make-op :action 'take-engl-4700
			:preconds '(hist-4700)
			:add-list '(engl-4700))
		(make-op :action 'take-engl-4750
			:preconds '(hist-4750)
			:add-list '(engl-4750))
		(make-op :action 'take-engl-4900
			:add-list '(engl-4900))
		(make-op :action 'take-engl-4910
			:add-list '(engl-4910))
		(make-op :action 'take-engl-5210
			:add-list '(engl-5210))
		(make-op :action 'take-engl-5300
			:preconds '(engl-2600)
			:add-list '(engl-5300))
		(make-op :action 'take-engl-5310
			:preconds '(engl-2600)
			:add-list '(engl-5310))
		(make-op :action 'take-engl-5320
			:preconds '(engl-2600)
			:add-list '(engl-5320))
		(make-op :action 'take-engl-5330
			:preconds '(engl-2600)
			:add-list '(engl-5330))
		(make-op :action 'take-engl-5340
			:preconds '(engl-2600)
			:add-list '(engl-5340))
		(make-op :action 'take-engl-5450
			:preconds '(engl-4420 engl-4430 engl-4440)
			:add-list '(engl-5450))
		(make-op :action 'take-engl-5690
			:preconds '(hist-5690)
			:add-list '(engl-5690))
		(make-op :action 'take-engl-5700
			:preconds '(anth-5700 hist-5700)
			:add-list '(engl-5700))
		(make-op :action 'take-engl-5900
			:add-list '(engl-5900))
		(make-op :action 'take-engl-5910
			:add-list '(engl-5910))
		(make-op :action 'take-engl-5920
			:add-list '(engl-5920))
		(make-op :action 'take-tcr-2100
			:add-list '(tcr-2100))
		(make-op :action 'take-tcr-2110
			:add-list '(tcr-2110))
		(make-op :action 'take-tcr-3100
			:add-list '(tcr-3100))
		(make-op :action 'take-tcr-3110
			:add-list '(tcr-3110))
		(make-op :action 'take-tcr-3120
			:add-list '(tcr-3120))
		(make-op :action 'take-tcr-3210
			:add-list '(tcr-3210))
		(make-op :action 'take-tcr-3220
			:add-list '(tcr-3220))
		(make-op :action 'take-tcr-3230
			:add-list '(tcr-3230))
		(make-op :action 'take-tcr-4210
			:preconds '(tcr-2110)
			:add-list '(tcr-4210))
		(make-op :action 'take-tcr-4220
			:add-list '(tcr-4220))
		(make-op :action 'take-tcr-4230
			:add-list '(tcr-4230))
		(make-op :action 'take-tcr-4240
			:add-list '(tcr-4240))
		(make-op :action 'take-tcr-4250
			:add-list '(tcr-4250))
		(make-op :action 'take-tcr-5490
			:add-list '(tcr-5490))
		(make-op :action 'take-esol-1030
			:add-list '(esol-1030))
		(make-op :action 'take-esol-1040
			:add-list '(esol-1040))
		(make-op :action 'take-esol-1050
			:add-list '(esol-1050))
		(make-op :action 'take-esol-1055
			:add-list '(esol-1055))
		(make-op :action 'take-esol-1060
			:add-list '(esol-1060))
		(make-op :action 'take-esol-1065
			:add-list '(esol-1065))
		(make-op :action 'take-esol-1140
			:add-list '(esol-1140))
		(make-op :action 'take-esol-1150
			:add-list '(esol-1150))
		(make-op :action 'take-esol-1155
			:add-list '(esol-1155))
		(make-op :action 'take-esol-1160
			:add-list '(esol-1160))
		(make-op :action 'take-esol-1165
			:add-list '(esol-1165))
		(make-op :action 'take-esol-2410
			:add-list '(esol-2410))
		(make-op :action 'take-esol-2420
			:add-list '(esol-2420))
		(make-op :action 'take-esol-2440
			:add-list '(esol-2440))
		(make-op :action 'take-esol-2460
			:add-list '(esol-2460))
		(make-op :action 'take-esol-2470
			:add-list '(esol-2470))
		(make-op :action 'take-envs-1200
			:add-list '(envs-1200))
		(make-op :action 'take-envs-1350
			:add-list '(envs-1350))
		(make-op :action 'take-envs-2000
			:preconds '(wats-2000 wild-2000)
			:add-list '(envs-2000))
		(make-op :action 'take-envs-2100
			:add-list '(envs-2100))
		(make-op :action 'take-envs-2250
			:add-list '(envs-2250))
		(make-op :action 'take-envs-2340
			:preconds '(geog-2340)
			:add-list '(envs-2340))
		(make-op :action 'take-envs-3000
			:preconds '(wild-2200)
			:add-list '(envs-3000))
		(make-op :action 'take-envs-3010
			:add-list '(envs-3010))
		(make-op :action 'take-envs-3300
			:add-list '(envs-3300))
		(make-op :action 'take-envs-3320
			:preconds '(anth-3320)
			:add-list '(envs-3320))
		(make-op :action 'take-envs-3330
			:add-list '(envs-3330))
		(make-op :action 'take-envs-3400
			:add-list '(envs-3400))
		(make-op :action 'take-envs-3500
			:preconds '(stat-1040 stat-1045)
			:add-list '(envs-3500))
		(make-op :action 'take-envs-3600
			:add-list '(envs-3600))
		(make-op :action 'take-envs-4000
			:add-list '(envs-4000))
		(make-op :action 'take-envs-4020
			:preconds '(engl-2010)
			:add-list '(envs-4020))
		(make-op :action 'take-envs-4030
			:preconds '(envs-6030)
			:add-list '(envs-4030))
		(make-op :action 'take-envs-4100
			:preconds '(envs-2340 envs-3500)
			:add-list '(envs-4100))
		(make-op :action 'take-envs-4110
			:preconds '(envs-6110)
			:add-list '(envs-4110))
		(make-op :action 'take-envs-4130
			:add-list '(envs-4130))
		(make-op :action 'take-envs-4200
			:add-list '(envs-4200))
		(make-op :action 'take-envs-4210
			:preconds '(anth-4210)
			:add-list '(envs-4210))
		(make-op :action 'take-envs-4250
			:add-list '(envs-4250))
		(make-op :action 'take-envs-4350
			:preconds '(envs-6350)
			:add-list '(envs-4350))
		(make-op :action 'take-envs-4500
			:preconds '(engl-2010 envs-3300)
			:add-list '(envs-4500))
		(make-op :action 'take-envs-4550
			:preconds '(envs-3300 stat-1040 stat-1045 stat-2000)
			:add-list '(envs-4550))
		(make-op :action 'take-envs-4600
			:add-list '(envs-4600))
		(make-op :action 'take-envs-4610
			:preconds '(envs-6610)
			:add-list '(envs-4610))
		(make-op :action 'take-envs-4620
			:preconds '(envs-6620)
			:add-list '(envs-4620))
		(make-op :action 'take-envs-4700
			:preconds '(engl-2010)
			:add-list '(envs-4700))
		(make-op :action 'take-envs-4920
			:add-list '(envs-4920))
		(make-op :action 'take-envs-4950
			:add-list '(envs-4950))
		(make-op :action 'take-envs-4960
			:add-list '(envs-4960))
		(make-op :action 'take-envs-4970
			:add-list '(envs-4970))
		(make-op :action 'take-envs-4980
			:add-list '(envs-4980))
		(make-op :action 'take-envs-5000
			:add-list '(envs-5000))
		(make-op :action 'take-envs-5300
			:add-list '(envs-5300))
		(make-op :action 'take-envs-5550
			:preconds '(envs-6550)
			:add-list '(envs-5550))
		(make-op :action 'take-envs-5820
			:add-list '(envs-5820))
		(make-op :action 'take-fcse-1021
			:add-list '(fcse-1021))
		(make-op :action 'take-fcse-1040
			:add-list '(fcse-1040))
		(make-op :action 'take-fcse-1140
			:add-list '(fcse-1140))
		(make-op :action 'take-fcse-1350
			:add-list '(fcse-1350))
		(make-op :action 'take-fcse-2040
			:preconds '(fcse-1040 fcse-1140)
			:add-list '(fcse-2040))
		(make-op :action 'take-fcse-2350
			:preconds '(fcse-1350 hdfs-3350)
			:add-list '(fcse-2350))
		(make-op :action 'take-fcse-2510
			:add-list '(fcse-2510))
		(make-op :action 'take-fcse-2520
			:add-list '(fcse-2520))
		(make-op :action 'take-fcse-2700
			:add-list '(fcse-2700))
		(make-op :action 'take-fcse-3030
			:add-list '(fcse-3030))
		(make-op :action 'take-fcse-3040
			:preconds '(fcse-2040)
			:add-list '(fcse-3040))
		(make-op :action 'take-fcse-3080
			:add-list '(fcse-3080))
		(make-op :action 'take-fcse-3140
			:preconds '(fcse-3040)
			:add-list '(fcse-3140))
		(make-op :action 'take-fcse-3200
			:add-list '(fcse-3200))
		(make-op :action 'take-fcse-3300
			:preconds '(fcse-3400)
			:add-list '(fcse-3300))
		(make-op :action 'take-fcse-3400
			:preconds '(fcse-3300)
			:add-list '(fcse-3400))
		(make-op :action 'take-fcse-3790
			:preconds '(fcse-2700)
			:add-list '(fcse-3790))
		(make-op :action 'take-fcse-4000
			:add-list '(fcse-4000))
		(make-op :action 'take-fcse-4030
			:preconds '(fcse-3030)
			:add-list '(fcse-4030))
		(make-op :action 'take-fcse-4040
			:preconds '(fcse-3080)
			:add-list '(fcse-4040))
		(make-op :action 'take-fcse-4140
			:preconds '(fcse-3030)
			:add-list '(fcse-4140))
		(make-op :action 'take-fcse-4210
			:preconds '(aste-4210 bsed-4210 tee-4210)
			:add-list '(fcse-4210))
		(make-op :action 'take-fcse-4250
			:add-list '(fcse-4250))
		(make-op :action 'take-fcse-4300
			:preconds '(fcse-3300 fcse-3400)
			:add-list '(fcse-4300))
		(make-op :action 'take-fcse-4400
			:preconds '(fcse-3300 fcse-3400)
			:add-list '(fcse-4400))
		(make-op :action 'take-fcse-4900
			:add-list '(fcse-4900))
		(make-op :action 'take-fcse-5500
			:preconds '(fcse-4300 fcse-4400 fcse-5630)
			:add-list '(fcse-5500))
		(make-op :action 'take-fcse-5550
			:add-list '(fcse-5550))
		(make-op :action 'take-fcse-5630
			:preconds '(fcse-4300 fcse-4400 fcse-5500)
			:add-list '(fcse-5630))
		(make-op :action 'take-film-1010
			:add-list '(film-1010))
		(make-op :action 'take-film-3220
			:preconds '(arth-2710 arth-2720)
			:add-list '(film-3220))
		(make-op :action 'take-film-3300
			:add-list '(film-3300))
		(make-op :action 'take-film-3310
			:add-list '(film-3310))
		(make-op :action 'take-film-3320
			:add-list '(film-3320))
		(make-op :action 'take-film-3330
			:add-list '(film-3330))
		(make-op :action 'take-film-3340
			:add-list '(film-3340))
		(make-op :action 'take-film-3350
			:add-list '(film-3350))
		(make-op :action 'take-film-3360
			:add-list '(film-3360))
		(make-op :action 'take-film-3370
			:add-list '(film-3370))
		(make-op :action 'take-film-4850
			:add-list '(film-4850))
		(make-op :action 'take-fin-1010
			:add-list '(fin-1010))
		(make-op :action 'take-fin-2000
			:add-list '(fin-2000))
		(make-op :action 'take-fin-3010
			:add-list '(fin-3010))
		(make-op :action 'take-fin-3020
			:add-list '(fin-3020))
		(make-op :action 'take-fin-3200
			:add-list '(fin-3200))
		(make-op :action 'take-fin-3300
			:preconds '(fin-3200)
			:add-list '(fin-3300))
		(make-op :action 'take-fin-3400
			:preconds '(fin-3200)
			:add-list '(fin-3400))
		(make-op :action 'take-fin-3460
			:add-list '(fin-3460))
		(make-op :action 'take-fin-3500
			:preconds '(fin-3200)
			:add-list '(fin-3500))
		(make-op :action 'take-fin-4200
			:preconds '(fin-3200)
			:add-list '(fin-4200))
		(make-op :action 'take-fin-4250
			:preconds '(fin-3200)
			:add-list '(fin-4250))
		(make-op :action 'take-fin-4300
			:preconds '(fin-3200)
			:add-list '(fin-4300))
		(make-op :action 'take-fin-4410
			:preconds '(fin-3200)
			:add-list '(fin-4410))
		(make-op :action 'take-fin-4420
			:preconds '(fin-3200)
			:add-list '(fin-4420))
		(make-op :action 'take-fin-4450
			:preconds '(fin-4200)
			:add-list '(fin-4450))
		(make-op :action 'take-fin-4460
			:preconds '(fin-3200)
			:add-list '(fin-4460))
		(make-op :action 'take-fin-4470
			:preconds '(fin-3200)
			:add-list '(fin-4470))
		(make-op :action 'take-fin-4475
			:preconds '(fin-3200 fin-4470)
			:add-list '(fin-4475))
		(make-op :action 'take-fin-4480
			:preconds '(fin-3200 math-1050 stat-1040 stat-2000 stat-2300 stat-3000 data-3100 psy-3010)
			:add-list '(fin-4480))
		(make-op :action 'take-fin-4490
			:preconds '(fin-3200)
			:add-list '(fin-4490))
		(make-op :action 'take-fin-4495
			:preconds '(fin-3200)
			:add-list '(fin-4495))
		(make-op :action 'take-fin-4530
			:preconds '(fin-3500)
			:add-list '(fin-4530))
		(make-op :action 'take-fin-4540
			:preconds '(fin-3200 fin-3500)
			:add-list '(fin-4540))
		(make-op :action 'take-fin-4550
			:preconds '(fin-3200 fin-3500)
			:add-list '(fin-4550))
		(make-op :action 'take-fin-4560
			:preconds '(fin-3200)
			:add-list '(fin-4560))
		(make-op :action 'take-fin-4900
			:add-list '(fin-4900))
		(make-op :action 'take-fin-4950
			:add-list '(fin-4950))
		(make-op :action 'take-fin-5000
			:preconds '(fin-3300)
			:add-list '(fin-5000))
		(make-op :action 'take-fin-5050
			:preconds '(fin-3300 fin-6050)
			:add-list '(fin-5050))
		(make-op :action 'take-fin-5060
			:preconds '(fin-6060)
			:add-list '(fin-5060))
		(make-op :action 'take-fin-5070
			:preconds '(acct-3520 fin-5060 fin-6070)
			:add-list '(fin-5070))
		(make-op :action 'take-fin-5080
			:preconds '(acct-3520 fin-5060 fin-6080)
			:add-list '(fin-5080))
		(make-op :action 'take-fin-5090
			:preconds '(acct-3520 fin-5060 fin-6090)
			:add-list '(fin-5090))
		(make-op :action 'take-fin-5095
			:preconds '(ecn-5090 ecn-6090 fin-6095)
			:add-list '(fin-5095))
		(make-op :action 'take-fin-5100
			:preconds '(fin-3300)
			:add-list '(fin-5100))
		(make-op :action 'take-fin-5110
			:preconds '(fin-3300 fin-6110)
			:add-list '(fin-5110))
		(make-op :action 'take-fin-5120
			:preconds '(fin-3300)
			:add-list '(fin-5120))
		(make-op :action 'take-fin-5250
			:preconds '(fin-4410)
			:add-list '(fin-5250))
		(make-op :action 'take-fin-5300
			:preconds '(fin-4410 fin-4460 fin-6300)
			:add-list '(fin-5300))
		(make-op :action 'take-fin-5320
			:preconds '(ecn-4330 fin-6320)
			:add-list '(fin-5320))
		(make-op :action 'take-fin-5330
			:preconds '(ecn-4330 fin-6330)
			:add-list '(fin-5330))
		(make-op :action 'take-fin-5350
			:preconds '(fin-3200)
			:add-list '(fin-5350))
		(make-op :action 'take-fin-5375
			:add-list '(fin-5375))
		(make-op :action 'take-fin-5440
			:preconds '(fin-4200)
			:add-list '(fin-5440))
		(make-op :action 'take-fin-5470
			:preconds '(fin-3200 fin-3300 fin-5300)
			:add-list '(fin-5470))
		(make-op :action 'take-fin-5475
			:preconds '(fin-5470)
			:add-list '(fin-5475))
		(make-op :action 'take-fin-5575
			:add-list '(fin-5575))
		(make-op :action 'take-fin-5600
			:preconds '(fin-3200)
			:add-list '(fin-5600))
		(make-op :action 'take-fin-5610
			:preconds '(fin-3200)
			:add-list '(fin-5610))
		(make-op :action 'take-fin-5700
			:preconds '(fin-3200)
			:add-list '(fin-5700))
		(make-op :action 'take-fin-5750
			:preconds '(fin-3300 ecn-3000 data-3100 stat-2300)
			:add-list '(fin-5750))
		(make-op :action 'take-fin-5800
			:add-list '(fin-5800))
		(make-op :action 'take-fren-1010
			:add-list '(fren-1010))
		(make-op :action 'take-fren-1020
			:preconds '(fren-1010)
			:add-list '(fren-1020))
		(make-op :action 'take-fren-1030
			:preconds '(fren-1010 fren-1020)
			:add-list '(fren-1030))
		(make-op :action 'take-fren-1050
			:add-list '(fren-1050))
		(make-op :action 'take-fren-1150
			:preconds '(fren-1010 fren-1050)
			:add-list '(fren-1150))
		(make-op :action 'take-fren-1820
			:add-list '(fren-1820))
		(make-op :action 'take-fren-2010
			:preconds '(fren-1020)
			:add-list '(fren-2010))
		(make-op :action 'take-fren-2020
			:preconds '(fren-2010)
			:add-list '(fren-2020))
		(make-op :action 'take-fren-2030
			:preconds '(fren-2010 fren-2020)
			:add-list '(fren-2030))
		(make-op :action 'take-fren-2050
			:preconds '(fren-1020)
			:add-list '(fren-2050))
		(make-op :action 'take-fren-2150
			:preconds '(fren-2010 fren-2050)
			:add-list '(fren-2150))
		(make-op :action 'take-fren-2820
			:add-list '(fren-2820))
		(make-op :action 'take-fren-2880
			:preconds '(fren-2010 fren-2020)
			:add-list '(fren-2880))
		(make-op :action 'take-fren-2988
			:add-list '(fren-2988))
		(make-op :action 'take-fren-3030
			:preconds '(fren-2020)
			:add-list '(fren-3030))
		(make-op :action 'take-fren-3060
			:preconds '(fren-2020)
			:add-list '(fren-3060))
		(make-op :action 'take-fren-3070
			:preconds '(fren-2020)
			:add-list '(fren-3070))
		(make-op :action 'take-fren-3080
			:preconds '(fren-2020)
			:add-list '(fren-3080))
		(make-op :action 'take-fren-3090
			:preconds '(fren-2020)
			:add-list '(fren-3090))
		(make-op :action 'take-fren-3116
			:add-list '(fren-3116))
		(make-op :action 'take-fren-3117
			:add-list '(fren-3117))
		(make-op :action 'take-fren-3118
			:add-list '(fren-3118))
		(make-op :action 'take-fren-3400
			:preconds '(fren-2020)
			:add-list '(fren-3400))
		(make-op :action 'take-fren-3500
			:add-list '(fren-3500))
		(make-op :action 'take-fren-3510
			:preconds '(fren-2020)
			:add-list '(fren-3510))
		(make-op :action 'take-fren-3550
			:preconds '(fren-2020)
			:add-list '(fren-3550))
		(make-op :action 'take-fren-3570
			:preconds '(fren-2020)
			:add-list '(fren-3570))
		(make-op :action 'take-fren-3600
			:preconds '(fren-2020)
			:add-list '(fren-3600))
		(make-op :action 'take-fren-3700
			:preconds '(fren-2020)
			:add-list '(fren-3700))
		(make-op :action 'take-fren-3820
			:preconds '(fren-2020)
			:add-list '(fren-3820))
		(make-op :action 'take-fren-3880
			:add-list '(fren-3880))
		(make-op :action 'take-fren-3900
			:preconds '(fren-2020)
			:add-list '(fren-3900))
		(make-op :action 'take-fren-4060
			:preconds '(fren-3060)
			:add-list '(fren-4060))
		(make-op :action 'take-fren-4090
			:preconds '(fren-3090)
			:add-list '(fren-4090))
		(make-op :action 'take-fren-4200
			:preconds '(fren-2020)
			:add-list '(fren-4200))
		(make-op :action 'take-fren-4520
			:preconds '(fren-2020)
			:add-list '(fren-4520))
		(make-op :action 'take-fren-4610
			:preconds '(fren-3600)
			:add-list '(fren-4610))
		(make-op :action 'take-fren-4620
			:preconds '(fren-3600)
			:add-list '(fren-4620))
		(make-op :action 'take-fren-4630
			:preconds '(fren-2020)
			:add-list '(fren-4630))
		(make-op :action 'take-fren-4640
			:preconds '(fren-2020)
			:add-list '(fren-4640))
		(make-op :action 'take-fren-4880
			:add-list '(fren-4880))
		(make-op :action 'take-fren-4900
			:preconds '(fren-2020)
			:add-list '(fren-4900))
		(make-op :action 'take-fren-4920
			:preconds '(fren-2020)
			:add-list '(fren-4920))
		(make-op :action 'take-geog-1000
			:add-list '(geog-1000))
		(make-op :action 'take-geog-1005
			:preconds '(geog-1000)
			:add-list '(geog-1005))
		(make-op :action 'take-geog-1300
			:add-list '(geog-1300))
		(make-op :action 'take-geog-1400
			:add-list '(geog-1400))
		(make-op :action 'take-geog-2340
			:preconds '(envs-2340)
			:add-list '(geog-2340))
		(make-op :action 'take-geog-2800
			:add-list '(geog-2800))
		(make-op :action 'take-geog-3100
			:preconds '(engl-2010)
			:add-list '(geog-3100))
		(make-op :action 'take-geog-3430
			:preconds '(pols-2100 pols-2200 pols-2300 pols-2400 pols-3430)
			:add-list '(geog-3430))
		(make-op :action 'take-geog-3800
			:preconds '(geog-2800 wild-2800)
			:add-list '(geog-3800))
		(make-op :action 'take-geog-4120
			:preconds '(engl-2010 geog-6120)
			:add-list '(geog-4120))
		(make-op :action 'take-geog-4210
			:add-list '(geog-4210))
		(make-op :action 'take-geog-4220
			:add-list '(geog-4220))
		(make-op :action 'take-geog-4400
			:preconds '(geog-6400)
			:add-list '(geog-4400))
		(make-op :action 'take-geog-4850
			:add-list '(geog-4850))
		(make-op :action 'take-geog-4860
			:preconds '(geog-2800 wild-2800)
			:add-list '(geog-4860))
		(make-op :action 'take-geog-4870
			:preconds '(geog-2800 wild-2800 wats-2800 stat-1080 envs-3500 data-3500 geog-4860 stat-1080 geog-6870)
			:add-list '(geog-4870))
		(make-op :action 'take-geog-4910
			:preconds '(geog-2800 wild-2800 wats-4930 geog-4860 geog-4870)
			:add-list '(geog-4910))
		(make-op :action 'take-geog-4920
			:preconds '(geog-2800 wild-2800)
			:add-list '(geog-4920))
		(make-op :action 'take-geog-4950
			:add-list '(geog-4950))
		(make-op :action 'take-geo-1010
			:add-list '(geo-1010))
		(make-op :action 'take-geo-1020
			:add-list '(geo-1020))
		(make-op :action 'take-geo-1060
			:add-list '(geo-1060))
		(make-op :action 'take-geo-1110
			:preconds '(geo-1115 geo-1115)
			:add-list '(geo-1110))
		(make-op :action 'take-geo-1115
			:preconds '(geo-1010 geo-1060 geo-1360 geo-1110)
			:add-list '(geo-1115))
		(make-op :action 'take-geo-1220
			:add-list '(geo-1220))
		(make-op :action 'take-geo-1225
			:add-list '(geo-1225))
		(make-op :action 'take-geo-1360
			:add-list '(geo-1360))
		(make-op :action 'take-geo-1380
			:add-list '(geo-1380))
		(make-op :action 'take-geo-1650
			:add-list '(geo-1650))
		(make-op :action 'take-geo-2200
			:preconds '(geo-1010 geo-1110)
			:add-list '(geo-2200))
		(make-op :action 'take-geo-2205
			:preconds '(geo-2200 geo-2200)
			:add-list '(geo-2205))
		(make-op :action 'take-geo-2500
			:preconds '(geo-1010 geo-1060 geo-1110 geo-1360)
			:add-list '(geo-2500))
		(make-op :action 'take-geo-2700
			:add-list '(geo-2700))
		(make-op :action 'take-geo-2800
			:preconds '(geo-1110 math-1060 math-1210 chem-1210)
			:add-list '(geo-2800))
		(make-op :action 'take-geo-2988
			:add-list '(geo-2988))
		(make-op :action 'take-geo-3100
			:add-list '(geo-3100))
		(make-op :action 'take-geo-3150
			:preconds '(phys-3150)
			:add-list '(geo-3150))
		(make-op :action 'take-geo-3250
			:add-list '(geo-3250))
		(make-op :action 'take-geo-3300
			:add-list '(geo-3300))
		(make-op :action 'take-geo-3400
			:preconds '(geo-1110 geo-2200 geo-2200)
			:add-list '(geo-3400))
		(make-op :action 'take-geo-3550
			:preconds '(geo-2200 math-1060 math-1210)
			:add-list '(geo-3550))
		(make-op :action 'take-geo-3600
			:preconds '(math-1060 math-1100 math-1210 phys-2210 wats-3600)
			:add-list '(geo-3600))
		(make-op :action 'take-geo-3700
			:preconds '(geo-3550 phys-2210 phys-2215)
			:add-list '(geo-3700))
		(make-op :action 'take-geo-3800
			:preconds '(geo-2200 geo-2800)
			:add-list '(geo-3800))
		(make-op :action 'take-geo-4500
			:preconds '(geo-2800)
			:add-list '(geo-4500))
		(make-op :action 'take-geo-4700
			:preconds '(geo-3600 geo-3700)
			:add-list '(geo-4700))
		(make-op :action 'take-geo-4800
			:add-list '(geo-4800))
		(make-op :action 'take-geo-4850
			:add-list '(geo-4850))
		(make-op :action 'take-geo-4900
			:add-list '(geo-4900))
		(make-op :action 'take-geo-5150
			:preconds '(wats-5150 geo-6150)
			:add-list '(geo-5150))
		(make-op :action 'take-geo-5200
			:preconds '(geo-2800 geo-3550 geo-3600 geo-3700 geo-4700)
			:add-list '(geo-5200))
		(make-op :action 'take-geo-5210
			:preconds '(geo-3800 geo-4500 geo-4700)
			:add-list '(geo-5210))
		(make-op :action 'take-geo-5360
			:preconds '(geo-6360)
			:add-list '(geo-5360))
		(make-op :action 'take-geo-5390
			:preconds '(geo-2800 geo-6390)
			:add-list '(geo-5390))
		(make-op :action 'take-geo-5410
			:preconds '(geo-3550 geo-6410)
			:add-list '(geo-5410))
		(make-op :action 'take-geo-5420
			:preconds '(geo-2800 geo-4500 geo-6420)
			:add-list '(geo-5420))
		(make-op :action 'take-geo-5430
			:preconds '(geo-2200 geo-6430)
			:add-list '(geo-5430))
		(make-op :action 'take-geo-5440
			:preconds '(geo-6440)
			:add-list '(geo-5440))
		(make-op :action 'take-geo-5460
			:preconds '(geo-2800 geo-3550 geo-6460)
			:add-list '(geo-5460))
		(make-op :action 'take-geo-5470
			:preconds '(geo-2800 geo-3550 geo-6470)
			:add-list '(geo-5470))
		(make-op :action 'take-geo-5480
			:preconds '(geo-2800 geo-3550 geo-6480)
			:add-list '(geo-5480))
		(make-op :action 'take-geo-5490
			:preconds '(geo-3550 geo-6490)
			:add-list '(geo-5490))
		(make-op :action 'take-geo-5500
			:preconds '(geo-4500 geo-6500)
			:add-list '(geo-5500))
		(make-op :action 'take-geo-5510
			:preconds '(geo-1110 math-1210 geo-3600 wats-3600)
			:add-list '(geo-5510))
		(make-op :action 'take-geo-5520
			:preconds '(geo-5510 geo-6520)
			:add-list '(geo-5520))
		(make-op :action 'take-geo-5530
			:preconds '(geo-3550 geo-3700)
			:add-list '(geo-5530))
		(make-op :action 'take-geo-5540
			:preconds '(geo-3550 geo-3600 math-1210 geo-6540)
			:add-list '(geo-5540))
		(make-op :action 'take-geo-5550
			:preconds '(chem-1210 geo-6550)
			:add-list '(geo-5550))
		(make-op :action 'take-geo-5560
			:preconds '(geo-3550 geo-3700 geo-6560)
			:add-list '(geo-5560))
		(make-op :action 'take-geo-5570
			:preconds '(geo-1110 math-1210)
			:add-list '(geo-5570))
		(make-op :action 'take-geo-5600
			:preconds '(chem-1210 chem-1220 geo-2800 math-1210)
			:add-list '(geo-5600))
		(make-op :action 'take-geo-5610
			:preconds '(geo-3700 geo-6610)
			:add-list '(geo-5610))
		(make-op :action 'take-geo-5620
			:preconds '(geo-3700 phys-2220 geo-6620)
			:add-list '(geo-5620))
		(make-op :action 'take-geo-5630
			:preconds '(geo-3600 geo-3700)
			:add-list '(geo-5630))
		(make-op :action 'take-geo-5640
			:add-list '(geo-5640))
		(make-op :action 'take-geo-5650
			:add-list '(geo-5650))
		(make-op :action 'take-geo-5660
			:preconds '(geo-6660 math-1210 phys-2210 phys-2220 geo-6660)
			:add-list '(geo-5660))
		(make-op :action 'take-geo-5670
			:add-list '(geo-5670))
		(make-op :action 'take-geo-5680
			:preconds '(geo-3600 wats-3600 psc-5680 wats-5680 geo-6680)
			:add-list '(geo-5680))
		(make-op :action 'take-geo-5690
			:add-list '(geo-5690))
		(make-op :action 'take-geo-5900
			:add-list '(geo-5900))
		(make-op :action 'take-germ-1010
			:add-list '(germ-1010))
		(make-op :action 'take-germ-1020
			:preconds '(germ-1010)
			:add-list '(germ-1020))
		(make-op :action 'take-germ-1800
			:add-list '(germ-1800))
		(make-op :action 'take-germ-2010
			:preconds '(germ-1020)
			:add-list '(germ-2010))
		(make-op :action 'take-germ-2020
			:preconds '(germ-2010)
			:add-list '(germ-2020))
		(make-op :action 'take-germ-2550
			:add-list '(germ-2550))
		(make-op :action 'take-germ-2570
			:add-list '(germ-2570))
		(make-op :action 'take-germ-2800
			:add-list '(germ-2800))
		(make-op :action 'take-germ-2880
			:add-list '(germ-2880))
		(make-op :action 'take-germ-2988
			:add-list '(germ-2988))
		(make-op :action 'take-germ-3000
			:preconds '(germ-2020)
			:add-list '(germ-3000))
		(make-op :action 'take-germ-3040
			:preconds '(germ-2020)
			:add-list '(germ-3040))
		(make-op :action 'take-germ-3050
			:preconds '(germ-2020)
			:add-list '(germ-3050))
		(make-op :action 'take-germ-3117
			:add-list '(germ-3117))
		(make-op :action 'take-germ-3300
			:preconds '(germ-2020)
			:add-list '(germ-3300))
		(make-op :action 'take-germ-3510
			:preconds '(germ-2020)
			:add-list '(germ-3510))
		(make-op :action 'take-germ-3540
			:preconds '(germ-2020)
			:add-list '(germ-3540))
		(make-op :action 'take-germ-3550
			:preconds '(germ-2020)
			:add-list '(germ-3550))
		(make-op :action 'take-germ-3600
			:preconds '(germ-2020)
			:add-list '(germ-3600))
		(make-op :action 'take-germ-3610
			:preconds '(germ-2020)
			:add-list '(germ-3610))
		(make-op :action 'take-germ-3750
			:preconds '(germ-2020)
			:add-list '(germ-3750))
		(make-op :action 'take-germ-3800
			:preconds '(germ-2020)
			:add-list '(germ-3800))
		(make-op :action 'take-germ-3880
			:add-list '(germ-3880))
		(make-op :action 'take-germ-4200
			:preconds '(germ-2020)
			:add-list '(germ-4200))
		(make-op :action 'take-germ-4600
			:preconds '(germ-2020)
			:add-list '(germ-4600))
		(make-op :action 'take-germ-4610
			:preconds '(germ-2020)
			:add-list '(germ-4610))
		(make-op :action 'take-germ-4650
			:preconds '(germ-2020)
			:add-list '(germ-4650))
		(make-op :action 'take-germ-4800
			:preconds '(germ-2020)
			:add-list '(germ-4800))
		(make-op :action 'take-germ-4880
			:add-list '(germ-4880))
		(make-op :action 'take-germ-4900
			:preconds '(germ-2020)
			:add-list '(germ-4900))
		(make-op :action 'take-germ-4910
			:preconds '(germ-2020)
			:add-list '(germ-4910))
		(make-op :action 'take-germ-4920
			:add-list '(germ-4920))
		(make-op :action 'take-grk-1010
			:preconds '(latn-1010 latn-1020)
			:add-list '(grk-1010))
		(make-op :action 'take-grk-1020
			:preconds '(grk-1010)
			:add-list '(grk-1020))
		(make-op :action 'take-grk-3300
			:preconds '(grk-1020)
			:add-list '(grk-3300))
		(make-op :action 'take-grk-3330
			:preconds '(grk-3300)
			:add-list '(grk-3330))
		(make-op :action 'take-grk-4300
			:preconds '(grk-3300 grk-3330)
			:add-list '(grk-4300))
		(make-op :action 'take-grk-4930
			:add-list '(grk-4930))
		(make-op :action 'take-heal-1000
			:add-list '(heal-1000))
		(make-op :action 'take-heal-1006
			:add-list '(heal-1006))
		(make-op :action 'take-heal-1008
			:add-list '(heal-1008))
		(make-op :action 'take-heal-1020
			:add-list '(heal-1020))
		(make-op :action 'take-heal-1030
			:add-list '(heal-1030))
		(make-op :action 'take-heal-1035
			:add-list '(heal-1035))
		(make-op :action 'take-heal-1110
			:add-list '(heal-1110))
		(make-op :action 'take-heal-1500
			:add-list '(heal-1500))
		(make-op :action 'take-heal-1600
			:add-list '(heal-1600))
		(make-op :action 'take-heal-1700
			:add-list '(heal-1700))
		(make-op :action 'take-heal-1701
			:add-list '(heal-1701))
		(make-op :action 'take-heal-1776
			:add-list '(heal-1776))
		(make-op :action 'take-heal-1835
			:add-list '(heal-1835))
		(make-op :action 'take-heal-1860
			:add-list '(heal-1860))
		(make-op :action 'take-heal-1878
			:add-list '(heal-1878))
		(make-op :action 'take-heal-1879
			:preconds '(heal-1878)
			:add-list '(heal-1879))
		(make-op :action 'take-heal-1900
			:add-list '(heal-1900))
		(make-op :action 'take-heal-2000
			:add-list '(heal-2000))
		(make-op :action 'take-heal-2020
			:add-list '(heal-2020))
		(make-op :action 'take-heal-2450
			:add-list '(heal-2450))
		(make-op :action 'take-heal-2977
			:add-list '(heal-2977))
		(make-op :action 'take-heal-2988
			:add-list '(heal-2988))
		(make-op :action 'take-hep-1500
			:add-list '(hep-1500))
		(make-op :action 'take-hep-2000
			:add-list '(hep-2000))
		(make-op :action 'take-hep-2300
			:add-list '(hep-2300))
		(make-op :action 'take-hep-2500
			:add-list '(hep-2500))
		(make-op :action 'take-hep-3000
			:add-list '(hep-3000))
		(make-op :action 'take-hep-3100
			:add-list '(hep-3100))
		(make-op :action 'take-hep-3200
			:add-list '(hep-3200))
		(make-op :action 'take-hep-3300
			:add-list '(hep-3300))
		(make-op :action 'take-hep-3400
			:add-list '(hep-3400))
		(make-op :action 'take-hep-3500
			:add-list '(hep-3500))
		(make-op :action 'take-hep-3600
			:add-list '(hep-3600))
		(make-op :action 'take-hep-3700
			:add-list '(hep-3700))
		(make-op :action 'take-hep-3800
			:add-list '(hep-3800))
		(make-op :action 'take-hep-3900
			:preconds '(hep-3600)
			:add-list '(hep-3900))
		(make-op :action 'take-hep-4000
			:add-list '(hep-4000))
		(make-op :action 'take-hep-4100
			:preconds '(hep-2500)
			:add-list '(hep-4100))
		(make-op :action 'take-hep-4200
			:preconds '(math-1030 math-1050 math-1051 stat-1040 stat-1045 ram-4200)
			:add-list '(hep-4200))
		(make-op :action 'take-hep-4205
			:preconds '(hep-4200 ram-4205)
			:add-list '(hep-4205))
		(make-op :action 'take-hep-4250
			:add-list '(hep-4250))
		(make-op :action 'take-hep-4300
			:add-list '(hep-4300))
		(make-op :action 'take-hep-4400
			:preconds '(hep-2500 hep-3600)
			:add-list '(hep-4400))
		(make-op :action 'take-hep-4500
			:add-list '(hep-4500))
		(make-op :action 'take-hep-4600
			:preconds '(hep-3600 hep-4100 hep-4200)
			:add-list '(hep-4600))
		(make-op :action 'take-hep-4700
			:add-list '(hep-4700))
		(make-op :action 'take-hep-4800
			:add-list '(hep-4800))
		(make-op :action 'take-hep-5000
			:preconds '(engl-2010 hep-2500 hep-4100)
			:add-list '(hep-5000))
		(make-op :action 'take-hep-5250
			:add-list '(hep-5250))
		(make-op :action 'take-hep-5300
			:preconds '(hep-2500)
			:add-list '(hep-5300))
		(make-op :action 'take-hep-5400
			:add-list '(hep-5400))
		(make-op :action 'take-hep-5500
			:preconds '(hep-4400)
			:add-list '(hep-5500))
		(make-op :action 'take-hep-5630
			:preconds '(hep-4400)
			:add-list '(hep-5630))
		(make-op :action 'take-hep-5700
			:add-list '(hep-5700))
		(make-op :action 'take-hep-5900
			:add-list '(hep-5900))
		(make-op :action 'take-hep-5950
			:add-list '(hep-5950))
		(make-op :action 'take-ram-4200
			:preconds '(math-1030 math-1050 math-1051 stat-1040 stat-1045 hep-4200)
			:add-list '(ram-4200))
		(make-op :action 'take-ram-4205
			:preconds '(hep-4200 hep-4205)
			:add-list '(ram-4205))
		(make-op :action 'take-hetr-1610
			:add-list '(hetr-1610))
		(make-op :action 'take-hetr-1620
			:add-list '(hetr-1620))
		(make-op :action 'take-hetr-1630
			:add-list '(hetr-1630))
		(make-op :action 'take-hetr-1640
			:add-list '(hetr-1640))
		(make-op :action 'take-hetr-1650
			:add-list '(hetr-1650))
		(make-op :action 'take-hetr-1660
			:add-list '(hetr-1660))
		(make-op :action 'take-hetr-1670
			:add-list '(hetr-1670))
		(make-op :action 'take-hetr-2750
			:preconds '(hetr-2790)
			:add-list '(hetr-2750))
		(make-op :action 'take-hetr-2760
			:add-list '(hetr-2760))
		(make-op :action 'take-hetr-2770
			:add-list '(hetr-2770))
		(make-op :action 'take-hetr-2780
			:add-list '(hetr-2780))
		(make-op :action 'take-hetr-2790
			:add-list '(hetr-2790))
		(make-op :action 'take-hetr-2977
			:add-list '(hetr-2977))
		(make-op :action 'take-hetr-2988
			:add-list '(hetr-2988))
		(make-op :action 'take-tehe-1010
			:add-list '(tehe-1010))
		(make-op :action 'take-tehe-1050
			:add-list '(tehe-1050))
		(make-op :action 'take-tehe-1100
			:add-list '(tehe-1100))
		(make-op :action 'take-tehe-1150
			:add-list '(tehe-1150))
		(make-op :action 'take-tehe-1200
			:add-list '(tehe-1200))
		(make-op :action 'take-tehe-1250
			:add-list '(tehe-1250))
		(make-op :action 'take-tehe-1300
			:add-list '(tehe-1300))
		(make-op :action 'take-tehe-1400
			:add-list '(tehe-1400))
		(make-op :action 'take-tehe-1500
			:add-list '(tehe-1500))
		(make-op :action 'take-hist-1100
			:add-list '(hist-1100))
		(make-op :action 'take-hist-1110
			:add-list '(hist-1110))
		(make-op :action 'take-hist-1120
			:preconds '(clas-1120)
			:add-list '(hist-1120))
		(make-op :action 'take-hist-1500
			:add-list '(hist-1500))
		(make-op :action 'take-hist-1510
			:add-list '(hist-1510))
		(make-op :action 'take-hist-1600
			:add-list '(hist-1600))
		(make-op :action 'take-hist-1700
			:add-list '(hist-1700))
		(make-op :action 'take-hist-2010
			:add-list '(hist-2010))
		(make-op :action 'take-hist-2018
			:add-list '(hist-2018))
		(make-op :action 'take-hist-2210
			:preconds '(anth-2210 engl-2210)
			:add-list '(hist-2210))
		(make-op :action 'take-hist-2700
			:add-list '(hist-2700))
		(make-op :action 'take-hist-2710
			:add-list '(hist-2710))
		(make-op :action 'take-hist-2720
			:preconds '(anth-2720 engl-2720)
			:add-list '(hist-2720))
		(make-op :action 'take-hist-2730
			:add-list '(hist-2730))
		(make-op :action 'take-hist-2977
			:add-list '(hist-2977))
		(make-op :action 'take-hist-2988
			:add-list '(hist-2988))
		(make-op :action 'take-hist-3000
			:preconds '(hist-4990 hist-1700)
			:add-list '(hist-3000))
		(make-op :action 'take-hist-3005
			:add-list '(hist-3005))
		(make-op :action 'take-hist-3010
			:preconds '(rels-3010)
			:add-list '(hist-3010))
		(make-op :action 'take-hist-3020
			:preconds '(rels-3020)
			:add-list '(hist-3020))
		(make-op :action 'take-hist-3030
			:preconds '(rels-3030)
			:add-list '(hist-3030))
		(make-op :action 'take-hist-3040
			:preconds '(igs-3040)
			:add-list '(hist-3040))
		(make-op :action 'take-hist-3060
			:preconds '(rels-3060)
			:add-list '(hist-3060))
		(make-op :action 'take-hist-3070
			:preconds '(engl-3070)
			:add-list '(hist-3070))
		(make-op :action 'take-hist-3080
			:preconds '(rels-3080)
			:add-list '(hist-3080))
		(make-op :action 'take-hist-3110
			:preconds '(arth-3110)
			:add-list '(hist-3110))
		(make-op :action 'take-hist-3130
			:add-list '(hist-3130))
		(make-op :action 'take-hist-3150
			:add-list '(hist-3150))
		(make-op :action 'take-hist-3160
			:preconds '(clas-3160)
			:add-list '(hist-3160))
		(make-op :action 'take-hist-3170
			:preconds '(clas-3170 rels-3170)
			:add-list '(hist-3170))
		(make-op :action 'take-hist-3210
			:preconds '(arth-3210 clas-3210 engl-3210 rels-3210)
			:add-list '(hist-3210))
		(make-op :action 'take-hist-3220
			:preconds '(rels-3220)
			:add-list '(hist-3220))
		(make-op :action 'take-hist-3225
			:add-list '(hist-3225))
		(make-op :action 'take-hist-3230
			:add-list '(hist-3230))
		(make-op :action 'take-hist-3240
			:add-list '(hist-3240))
		(make-op :action 'take-hist-3250
			:add-list '(hist-3250))
		(make-op :action 'take-hist-3270
			:preconds '(rels-3270)
			:add-list '(hist-3270))
		(make-op :action 'take-hist-3280
			:add-list '(hist-3280))
		(make-op :action 'take-hist-3290
			:preconds '(clas-3290 rels-3290)
			:add-list '(hist-3290))
		(make-op :action 'take-hist-3320
			:add-list '(hist-3320))
		(make-op :action 'take-hist-3330
			:add-list '(hist-3330))
		(make-op :action 'take-hist-3340
			:add-list '(hist-3340))
		(make-op :action 'take-hist-3350
			:add-list '(hist-3350))
		(make-op :action 'take-hist-3390
			:preconds '(anth-3390)
			:add-list '(hist-3390))
		(make-op :action 'take-hist-3410
			:preconds '(rels-3410)
			:add-list '(hist-3410))
		(make-op :action 'take-hist-3430
			:preconds '(rels-3430)
			:add-list '(hist-3430))
		(make-op :action 'take-hist-3450
			:add-list '(hist-3450))
		(make-op :action 'take-hist-3456
			:preconds '(rels-3456)
			:add-list '(hist-3456))
		(make-op :action 'take-hist-3460
			:add-list '(hist-3460))
		(make-op :action 'take-hist-3470
			:preconds '(rels-3470)
			:add-list '(hist-3470))
		(make-op :action 'take-hist-3482
			:preconds '(rels-3482)
			:add-list '(hist-3482))
		(make-op :action 'take-hist-3483
			:add-list '(hist-3483))
		(make-op :action 'take-hist-3510
			:add-list '(hist-3510))
		(make-op :action 'take-hist-3530
			:add-list '(hist-3530))
		(make-op :action 'take-hist-3550
			:add-list '(hist-3550))
		(make-op :action 'take-hist-3560
			:add-list '(hist-3560))
		(make-op :action 'take-hist-3615
			:add-list '(hist-3615))
		(make-op :action 'take-hist-3620
			:add-list '(hist-3620))
		(make-op :action 'take-hist-3630
			:add-list '(hist-3630))
		(make-op :action 'take-hist-3650
			:add-list '(hist-3650))
		(make-op :action 'take-hist-3660
			:add-list '(hist-3660))
		(make-op :action 'take-hist-3670
			:add-list '(hist-3670))
		(make-op :action 'take-hist-3700
			:preconds '(engl-3700)
			:add-list '(hist-3700))
		(make-op :action 'take-hist-3710
			:preconds '(anth-3710 engl-3710 rels-3710)
			:add-list '(hist-3710))
		(make-op :action 'take-hist-3720
			:add-list '(hist-3720))
		(make-op :action 'take-hist-3730
			:add-list '(hist-3730))
		(make-op :action 'take-hist-3740
			:add-list '(hist-3740))
		(make-op :action 'take-hist-3750
			:add-list '(hist-3750))
		(make-op :action 'take-hist-3751
			:add-list '(hist-3751))
		(make-op :action 'take-hist-3760
			:add-list '(hist-3760))
		(make-op :action 'take-hist-3770
			:add-list '(hist-3770))
		(make-op :action 'take-hist-3780
			:preconds '(teal-3780)
			:add-list '(hist-3780))
		(make-op :action 'take-hist-3830
			:preconds '(engl-3830 kin-3830 rels-3830)
			:add-list '(hist-3830))
		(make-op :action 'take-hist-3840
			:add-list '(hist-3840))
		(make-op :action 'take-hist-3850
			:add-list '(hist-3850))
		(make-op :action 'take-hist-3900
			:add-list '(hist-3900))
		(make-op :action 'take-hist-3910
			:add-list '(hist-3910))
		(make-op :action 'take-hist-3950
			:add-list '(hist-3950))
		(make-op :action 'take-hist-4015
			:preconds '(rels-4015)
			:add-list '(hist-4015))
		(make-op :action 'take-hist-4020
			:add-list '(hist-4020))
		(make-op :action 'take-hist-4150
			:add-list '(hist-4150))
		(make-op :action 'take-hist-4230
			:preconds '(rels-4230)
			:add-list '(hist-4230))
		(make-op :action 'take-hist-4250
			:add-list '(hist-4250))
		(make-op :action 'take-hist-4251
			:add-list '(hist-4251))
		(make-op :action 'take-hist-4260
			:add-list '(hist-4260))
		(make-op :action 'take-hist-4290
			:add-list '(hist-4290))
		(make-op :action 'take-hist-4310
			:add-list '(hist-4310))
		(make-op :action 'take-hist-4330
			:preconds '(hist-1110)
			:add-list '(hist-4330))
		(make-op :action 'take-hist-4350
			:preconds '(clas-3210 rels-4350)
			:add-list '(hist-4350))
		(make-op :action 'take-hist-4390
			:add-list '(hist-4390))
		(make-op :action 'take-hist-4510
			:add-list '(hist-4510))
		(make-op :action 'take-hist-4540
			:add-list '(hist-4540))
		(make-op :action 'take-hist-4550
			:add-list '(hist-4550))
		(make-op :action 'take-hist-4555
			:preconds '(rels-4555)
			:add-list '(hist-4555))
		(make-op :action 'take-hist-4560
			:add-list '(hist-4560))
		(make-op :action 'take-hist-4565
			:preconds '(rels-4565)
			:add-list '(hist-4565))
		(make-op :action 'take-hist-4566
			:preconds '(rels-4566)
			:add-list '(hist-4566))
		(make-op :action 'take-hist-4567
			:preconds '(rels-4567)
			:add-list '(hist-4567))
		(make-op :action 'take-hist-4600
			:add-list '(hist-4600))
		(make-op :action 'take-hist-4610
			:add-list '(hist-4610))
		(make-op :action 'take-hist-4620
			:preconds '(engl-4620)
			:add-list '(hist-4620))
		(make-op :action 'take-hist-4625
			:add-list '(hist-4625))
		(make-op :action 'take-hist-4630
			:add-list '(hist-4630))
		(make-op :action 'take-hist-4640
			:add-list '(hist-4640))
		(make-op :action 'take-hist-4650
			:add-list '(hist-4650))
		(make-op :action 'take-hist-4660
			:add-list '(hist-4660))
		(make-op :action 'take-hist-4700
			:preconds '(engl-4700)
			:add-list '(hist-4700))
		(make-op :action 'take-hist-4710
			:add-list '(hist-4710))
		(make-op :action 'take-hist-4711
			:add-list '(hist-4711))
		(make-op :action 'take-hist-4720
			:add-list '(hist-4720))
		(make-op :action 'take-hist-4730
			:preconds '(rels-4730)
			:add-list '(hist-4730))
		(make-op :action 'take-hist-4750
			:preconds '(engl-4750)
			:add-list '(hist-4750))
		(make-op :action 'take-hist-4760
			:add-list '(hist-4760))
		(make-op :action 'take-hist-4790
			:preconds '(rels-4790)
			:add-list '(hist-4790))
		(make-op :action 'take-hist-4810
			:preconds '(msl-4810)
			:add-list '(hist-4810))
		(make-op :action 'take-hist-4815
			:add-list '(hist-4815))
		(make-op :action 'take-hist-4820
			:add-list '(hist-4820))
		(make-op :action 'take-hist-4821
			:add-list '(hist-4821))
		(make-op :action 'take-hist-4822
			:add-list '(hist-4822))
		(make-op :action 'take-hist-4825
			:preconds '(rels-4825)
			:add-list '(hist-4825))
		(make-op :action 'take-hist-4850
			:add-list '(hist-4850))
		(make-op :action 'take-hist-4860
			:add-list '(hist-4860))
		(make-op :action 'take-hist-4870
			:add-list '(hist-4870))
		(make-op :action 'take-hist-4880
			:add-list '(hist-4880))
		(make-op :action 'take-hist-4885
			:add-list '(hist-4885))
		(make-op :action 'take-hist-4890
			:add-list '(hist-4890))
		(make-op :action 'take-hist-4891
			:add-list '(hist-4891))
		(make-op :action 'take-hist-4910
			:add-list '(hist-4910))
		(make-op :action 'take-hist-4930
			:add-list '(hist-4930))
		(make-op :action 'take-hist-4940
			:add-list '(hist-4940))
		(make-op :action 'take-hist-4945
			:preconds '(hist-6840)
			:add-list '(hist-4945))
		(make-op :action 'take-hist-4990
			:preconds '(hist-3000)
			:add-list '(hist-4990))
		(make-op :action 'take-hist-5690
			:preconds '(engl-5690)
			:add-list '(hist-5690))
		(make-op :action 'take-hist-5700
			:preconds '(anth-5700 engl-5700)
			:add-list '(hist-5700))
		(make-op :action 'take-msl-4810
			:preconds '(hist-4810)
			:add-list '(msl-4810))
		(make-op :action 'take-honr-1300
			:add-list '(honr-1300))
		(make-op :action 'take-honr-1320
			:add-list '(honr-1320))
		(make-op :action 'take-honr-1330
			:add-list '(honr-1330))
		(make-op :action 'take-honr-1340
			:add-list '(honr-1340))
		(make-op :action 'take-honr-1350
			:add-list '(honr-1350))
		(make-op :action 'take-honr-1360
			:add-list '(honr-1360))
		(make-op :action 'take-honr-2000
			:add-list '(honr-2000))
		(make-op :action 'take-honr-2100
			:add-list '(honr-2100))
		(make-op :action 'take-honr-2200
			:add-list '(honr-2200))
		(make-op :action 'take-honr-3010
			:add-list '(honr-3010))
		(make-op :action 'take-honr-3020
			:add-list '(honr-3020))
		(make-op :action 'take-honr-3030
			:add-list '(honr-3030))
		(make-op :action 'take-honr-3035
			:add-list '(honr-3035))
		(make-op :action 'take-honr-3070
			:preconds '(usu-3070)
			:add-list '(honr-3070))
		(make-op :action 'take-honr-3071
			:add-list '(honr-3071))
		(make-op :action 'take-honr-3072
			:add-list '(honr-3072))
		(make-op :action 'take-honr-3900
			:add-list '(honr-3900))
		(make-op :action 'take-honr-4000
			:add-list '(honr-4000))
		(make-op :action 'take-honr-4700
			:add-list '(honr-4700))
		(make-op :action 'take-honr-4800
			:add-list '(honr-4800))
		(make-op :action 'take-honr-4900
			:add-list '(honr-4900))
		(make-op :action 'take-hdfs-1010
			:add-list '(hdfs-1010))
		(make-op :action 'take-hdfs-1500
			:add-list '(hdfs-1500))
		(make-op :action 'take-hdfs-2000
			:add-list '(hdfs-2000))
		(make-op :action 'take-hdfs-2100
			:add-list '(hdfs-2100))
		(make-op :action 'take-hdfs-2200
			:add-list '(hdfs-2200))
		(make-op :action 'take-hdfs-2400
			:add-list '(hdfs-2400))
		(make-op :action 'take-hdfs-2450
			:add-list '(hdfs-2450))
		(make-op :action 'take-hdfs-2520
			:add-list '(hdfs-2520))
		(make-op :action 'take-hdfs-2600
			:preconds '(hdfs-2630)
			:add-list '(hdfs-2600))
		(make-op :action 'take-hdfs-2630
			:preconds '(hdfs-2600)
			:add-list '(hdfs-2630))
		(make-op :action 'take-hdfs-2660
			:add-list '(hdfs-2660))
		(make-op :action 'take-hdfs-3100
			:preconds '(hdfs-1500 hdfs-2400)
			:add-list '(hdfs-3100))
		(make-op :action 'take-hdfs-3110
			:preconds '(hdfs-1500 hdfs-2400)
			:add-list '(hdfs-3110))
		(make-op :action 'take-hdfs-3130
			:preconds '(stat-1040 stat-1045 psy-3010 soc-3120)
			:add-list '(hdfs-3130))
		(make-op :action 'take-hdfs-3210
			:preconds '(hdfs-1500 hdfs-2400)
			:add-list '(hdfs-3210))
		(make-op :action 'take-hdfs-3350
			:add-list '(hdfs-3350))
		(make-op :action 'take-hdfs-3450
			:preconds '(hdfs-3350)
			:add-list '(hdfs-3450))
		(make-op :action 'take-hdfs-3500
			:preconds '(hdfs-1500)
			:add-list '(hdfs-3500))
		(make-op :action 'take-hdfs-3540
			:preconds '(hdfs-1500)
			:add-list '(hdfs-3540))
		(make-op :action 'take-hdfs-3550
			:preconds '(hdfs-3500 hdfs-1500 hdfs-3500)
			:add-list '(hdfs-3550))
		(make-op :action 'take-hdfs-3560
			:preconds '(hdfs-3570 hdfs-1500 hdfs-3570)
			:add-list '(hdfs-3560))
		(make-op :action 'take-hdfs-3570
			:preconds '(hdfs-1500)
			:add-list '(hdfs-3570))
		(make-op :action 'take-hdfs-3580
			:preconds '(hdfs-3570 hdfs-3570)
			:add-list '(hdfs-3580))
		(make-op :action 'take-hdfs-3590
			:preconds '(hdfs-3540 hdfs-3540)
			:add-list '(hdfs-3590))
		(make-op :action 'take-hdfs-3700
			:preconds '(psy-3700)
			:add-list '(hdfs-3700))
		(make-op :action 'take-hdfs-4220
			:preconds '(hdfs-2400)
			:add-list '(hdfs-4220))
		(make-op :action 'take-hdfs-4230
			:preconds '(hdfs-2400)
			:add-list '(hdfs-4230))
		(make-op :action 'take-hdfs-4240
			:preconds '(hdfs-1500)
			:add-list '(hdfs-4240))
		(make-op :action 'take-hdfs-4250
			:add-list '(hdfs-4250))
		(make-op :action 'take-hdfs-4260
			:preconds '(hdfs-1500 hdfs-2400)
			:add-list '(hdfs-4260))
		(make-op :action 'take-hdfs-4270
			:add-list '(hdfs-4270))
		(make-op :action 'take-hdfs-4350
			:preconds '(hdfs-3350)
			:add-list '(hdfs-4350))
		(make-op :action 'take-hdfs-4400
			:add-list '(hdfs-4400))
		(make-op :action 'take-hdfs-4460
			:preconds '(hdfs-3350)
			:add-list '(hdfs-4460))
		(make-op :action 'take-hdfs-4550
			:preconds '(hdfs-1500)
			:add-list '(hdfs-4550))
		(make-op :action 'take-hdfs-4830
			:add-list '(hdfs-4830))
		(make-op :action 'take-hdfs-4900
			:add-list '(hdfs-4900))
		(make-op :action 'take-hdfs-4930
			:preconds '(psy-4930)
			:add-list '(hdfs-4930))
		(make-op :action 'take-hdfs-4940
			:add-list '(hdfs-4940))
		(make-op :action 'take-hdfs-4960
			:preconds '(hdfs-4550)
			:add-list '(hdfs-4960))
		(make-op :action 'take-hdfs-4970
			:preconds '(hdfs-3540 hdfs-4240)
			:add-list '(hdfs-4970))
		(make-op :action 'take-hdfs-4980
			:preconds '(hdfs-4900)
			:add-list '(hdfs-4980))
		(make-op :action 'take-hdfs-4990
			:add-list '(hdfs-4990))
		(make-op :action 'take-hdfs-5330
			:add-list '(hdfs-5330))
		(make-op :action 'take-hdfs-5340
			:preconds '(hdfs-3350)
			:add-list '(hdfs-5340))
		(make-op :action 'take-hdfs-5460
			:add-list '(hdfs-5460))
		(make-op :action 'take-hdfs-5550
			:add-list '(hdfs-5550))
		(make-op :action 'take-hdfs-5560
			:add-list '(hdfs-5560))
		(make-op :action 'take-hdfs-5950
			:preconds '(hdfs-4460 hdfs-5340 hdfs-5330)
			:add-list '(hdfs-5950))
		(make-op :action 'take-chss-1100
			:add-list '(chss-1100))
		(make-op :action 'take-chss-1250
			:add-list '(chss-1250))
		(make-op :action 'take-chss-2250
			:add-list '(chss-2250))
		(make-op :action 'take-chss-3050
			:add-list '(chss-3050))
		(make-op :action 'take-chss-3250
			:add-list '(chss-3250))
		(make-op :action 'take-chss-3550
			:add-list '(chss-3550))
		(make-op :action 'take-chss-4250
			:add-list '(chss-4250))
		(make-op :action 'take-chss-4900
			:add-list '(chss-4900))
		(make-op :action 'take-chss-5250
			:add-list '(chss-5250))
		(make-op :action 'take-chss-5950
			:add-list '(chss-5950))
		(make-op :action 'take-pi-1010
			:add-list '(pi-1010))
		(make-op :action 'take-pi-4940
			:add-list '(pi-4940))
		(make-op :action 'take-pi-4990
			:add-list '(pi-4990))
		(make-op :action 'take-is-3600
			:add-list '(is-3600))
		(make-op :action 'take-is-3700
			:preconds '(data-2100 oss-2450)
			:add-list '(is-3700))
		(make-op :action 'take-is-3800
			:add-list '(is-3800))
		(make-op :action 'take-is-4250
			:preconds '(data-3330 data-3500)
			:add-list '(is-4250))
		(make-op :action 'take-is-4830
			:preconds '(is-6830)
			:add-list '(is-4830))
		(make-op :action 'take-is-4950
			:add-list '(is-4950))
		(make-op :action 'take-is-5100
			:preconds '(data-3500 cs-1400 is-6100)
			:add-list '(is-5100))
		(make-op :action 'take-is-5150
			:preconds '(data-3330 data-3500)
			:add-list '(is-5150))
		(make-op :action 'take-is-5170
			:preconds '(is-6170)
			:add-list '(is-5170))
		(make-op :action 'take-is-5700
			:preconds '(data-3330 is-3700 cs-1400 data-3500 is-6700)
			:add-list '(is-5700))
		(make-op :action 'take-is-5750
			:preconds '(is-5700 is-6750)
			:add-list '(is-5750))
		(make-op :action 'take-is-5800
			:preconds '(data-3330 is-3800 is-4830 cs-1400 data-3500)
			:add-list '(is-5800))
		(make-op :action 'take-is-5820
			:preconds '(is-6820)
			:add-list '(is-5820))
		(make-op :action 'take-is-5850
			:preconds '(is-5800)
			:add-list '(is-5850))
		(make-op :action 'take-is-5880
			:preconds '(is-6880)
			:add-list '(is-5880))
		(make-op :action 'take-is-5900
			:preconds '(data-2100 is-3800 data-3500 cs-1400 is-5910)
			:add-list '(is-5900))
		(make-op :action 'take-is-5910
			:preconds '(is-5900 data-2100 data-3330 is-3800 data-3500 cs-1400 is-5900)
			:add-list '(is-5910))
		(make-op :action 'take-is-5950
			:add-list '(is-5950))
		(make-op :action 'take-iogp-4850
			:preconds '(cmst-1330 cmst-2110 pols-1100 pols-2100 pols-2200 pols-2400 pols-4850)
			:add-list '(iogp-4850))
		(make-op :action 'take-iogp-5900
			:add-list '(iogp-5900))
		(make-op :action 'take-iogp-5910
			:add-list '(iogp-5910))
		(make-op :action 'take-iogp-5920
			:add-list '(iogp-5920))
		(make-op :action 'take-iogp-5930
			:add-list '(iogp-5930))
		(make-op :action 'take-iogp-5940
			:add-list '(iogp-5940))
		(make-op :action 'take-iogp-5950
			:add-list '(iogp-5950))
		(make-op :action 'take-itls-1870
			:add-list '(itls-1870))
		(make-op :action 'take-itls-2890
			:add-list '(itls-2890))
		(make-op :action 'take-itls-2900
			:add-list '(itls-2900))
		(make-op :action 'take-itls-3000
			:add-list '(itls-3000))
		(make-op :action 'take-itls-3110
			:add-list '(itls-3110))
		(make-op :action 'take-itls-3120
			:preconds '(itls-3110)
			:add-list '(itls-3120))
		(make-op :action 'take-itls-3130
			:add-list '(itls-3130))
		(make-op :action 'take-itls-3310
			:add-list '(itls-3310))
		(make-op :action 'take-itls-3350
			:add-list '(itls-3350))
		(make-op :action 'take-itls-3500
			:add-list '(itls-3500))
		(make-op :action 'take-itls-3530
			:add-list '(itls-3530))
		(make-op :action 'take-itls-3560
			:add-list '(itls-3560))
		(make-op :action 'take-itls-3575
			:add-list '(itls-3575))
		(make-op :action 'take-itls-3870
			:add-list '(itls-3870))
		(make-op :action 'take-itls-4110
			:add-list '(itls-4110))
		(make-op :action 'take-itls-4130
			:preconds '(stat-1040)
			:add-list '(itls-4130))
		(make-op :action 'take-itls-4160
			:add-list '(itls-4160))
		(make-op :action 'take-itls-4205
			:add-list '(itls-4205))
		(make-op :action 'take-itls-4210
			:preconds '(itls-6210)
			:add-list '(itls-4210))
		(make-op :action 'take-itls-4215
			:preconds '(itls-6215)
			:add-list '(itls-4215))
		(make-op :action 'take-itls-4220
			:preconds '(itls-4215 itls-6215 itls-6220)
			:add-list '(itls-4220))
		(make-op :action 'take-itls-4225
			:add-list '(itls-4225))
		(make-op :action 'take-itls-4230
			:preconds '(itls-6230)
			:add-list '(itls-4230))
		(make-op :action 'take-itls-4240
			:preconds '(itls-6240)
			:add-list '(itls-4240))
		(make-op :action 'take-itls-4245
			:preconds '(itls-6245)
			:add-list '(itls-4245))
		(make-op :action 'take-itls-4260
			:preconds '(itls-6260)
			:add-list '(itls-4260))
		(make-op :action 'take-itls-4265
			:preconds '(itls-6265)
			:add-list '(itls-4265))
		(make-op :action 'take-itls-4270
			:preconds '(itls-6270)
			:add-list '(itls-4270))
		(make-op :action 'take-itls-4275
			:preconds '(itls-6275)
			:add-list '(itls-4275))
		(make-op :action 'take-itls-4285
			:preconds '(itls-6285)
			:add-list '(itls-4285))
		(make-op :action 'take-itls-4300
			:add-list '(itls-4300))
		(make-op :action 'take-itls-4320
			:preconds '(itls-6320)
			:add-list '(itls-4320))
		(make-op :action 'take-itls-4410
			:add-list '(itls-4410))
		(make-op :action 'take-itls-4420
			:add-list '(itls-4420))
		(make-op :action 'take-itls-4475
			:add-list '(itls-4475))
		(make-op :action 'take-itls-4890
			:add-list '(itls-4890))
		(make-op :action 'take-itls-4900
			:add-list '(itls-4900))
		(make-op :action 'take-itls-4910
			:add-list '(itls-4910))
		(make-op :action 'take-itls-4940
			:add-list '(itls-4940))
		(make-op :action 'take-itls-4960
			:add-list '(itls-4960))
		(make-op :action 'take-itls-5000
			:add-list '(itls-5000))
		(make-op :action 'take-itls-5015
			:add-list '(itls-5015))
		(make-op :action 'take-itls-5025
			:add-list '(itls-5025))
		(make-op :action 'take-itls-5030
			:add-list '(itls-5030))
		(make-op :action 'take-itls-5040
			:add-list '(itls-5040))
		(make-op :action 'take-itls-5220
			:preconds '(itls-4215 itls-6215)
			:add-list '(itls-5220))
		(make-op :action 'take-itls-5230
			:preconds '(itls-6230)
			:add-list '(itls-5230))
		(make-op :action 'take-itls-5500
			:add-list '(itls-5500))
		(make-op :action 'take-itls-5700
			:add-list '(itls-5700))
		(make-op :action 'take-itls-5750
			:add-list '(itls-5750))
		(make-op :action 'take-istu-2250
			:add-list '(istu-2250))
		(make-op :action 'take-istu-3010
			:add-list '(istu-3010))
		(make-op :action 'take-istu-4010
			:preconds '(istu-3010)
			:add-list '(istu-4010))
		(make-op :action 'take-istu-4250
			:add-list '(istu-4250))
		(make-op :action 'take-ieli-1000
			:add-list '(ieli-1000))
		(make-op :action 'take-ieli-1120
			:add-list '(ieli-1120))
		(make-op :action 'take-ieli-1160
			:add-list '(ieli-1160))
		(make-op :action 'take-ieli-1220
			:add-list '(ieli-1220))
		(make-op :action 'take-ieli-1230
			:add-list '(ieli-1230))
		(make-op :action 'take-ieli-1240
			:add-list '(ieli-1240))
		(make-op :action 'take-ieli-1260
			:add-list '(ieli-1260))
		(make-op :action 'take-ieli-1900
			:add-list '(ieli-1900))
		(make-op :action 'take-ieli-2310
			:add-list '(ieli-2310))
		(make-op :action 'take-ieli-2320
			:add-list '(ieli-2320))
		(make-op :action 'take-ieli-2330
			:add-list '(ieli-2330))
		(make-op :action 'take-ieli-2360
			:add-list '(ieli-2360))
		(make-op :action 'take-ieli-2410
			:add-list '(ieli-2410))
		(make-op :action 'take-ieli-2420
			:preconds '(lang-2420)
			:add-list '(ieli-2420))
		(make-op :action 'take-ieli-2440
			:add-list '(ieli-2440))
		(make-op :action 'take-ieli-2450
			:add-list '(ieli-2450))
		(make-op :action 'take-ieli-2460
			:add-list '(ieli-2460))
		(make-op :action 'take-ieli-2470
			:add-list '(ieli-2470))
		(make-op :action 'take-ieli-2475
			:add-list '(ieli-2475))
		(make-op :action 'take-ieli-2900
			:add-list '(ieli-2900))
		(make-op :action 'take-itds-4900
			:add-list '(itds-4900))
		(make-op :action 'take-iad-1700
			:add-list '(iad-1700))
		(make-op :action 'take-iad-1750
			:add-list '(iad-1750))
		(make-op :action 'take-iad-1760
			:add-list '(iad-1760))
		(make-op :action 'take-iad-1790
			:add-list '(iad-1790))
		(make-op :action 'take-iad-2710
			:add-list '(iad-2710))
		(make-op :action 'take-iad-2720
			:preconds '(iad-2710)
			:add-list '(iad-2720))
		(make-op :action 'take-iad-2730
			:preconds '(iad-2710)
			:add-list '(iad-2730))
		(make-op :action 'take-iad-2750
			:add-list '(iad-2750))
		(make-op :action 'take-iad-2760
			:preconds '(iad-2750)
			:add-list '(iad-2760))
		(make-op :action 'take-iad-2770
			:add-list '(iad-2770))
		(make-op :action 'take-iad-2780
			:add-list '(iad-2780))
		(make-op :action 'take-iad-3710
			:preconds '(iad-4710)
			:add-list '(iad-3710))
		(make-op :action 'take-iad-3730
			:preconds '(iad-2730)
			:add-list '(iad-3730))
		(make-op :action 'take-iad-3740
			:preconds '(iad-3730 iad-3760)
			:add-list '(iad-3740))
		(make-op :action 'take-iad-3760
			:preconds '(iad-2730)
			:add-list '(iad-3760))
		(make-op :action 'take-iad-3770
			:preconds '(iad-3760)
			:add-list '(iad-3770))
		(make-op :action 'take-iad-3790
			:add-list '(iad-3790))
		(make-op :action 'take-iad-4700
			:add-list '(iad-4700))
		(make-op :action 'take-iad-4710
			:preconds '(iad-3710 iad-3760 iad-3770)
			:add-list '(iad-4710))
		(make-op :action 'take-iad-4730
			:add-list '(iad-4730))
		(make-op :action 'take-iad-4740
			:preconds '(iad-4710)
			:add-list '(iad-4740))
		(make-op :action 'take-iad-4750
			:preconds '(iad-3770 iad-4790)
			:add-list '(iad-4750))
		(make-op :action 'take-iad-4760
			:preconds '(iad-4750)
			:add-list '(iad-4760))
		(make-op :action 'take-iad-4770
			:preconds '(iad-4750)
			:add-list '(iad-4770))
		(make-op :action 'take-iad-4780
			:add-list '(iad-4780))
		(make-op :action 'take-iad-4790
			:add-list '(iad-4790))
		(make-op :action 'take-iad-4900
			:add-list '(iad-4900))
		(make-op :action 'take-iad-4910
			:add-list '(iad-4910))
		(make-op :action 'take-igs-1010
			:add-list '(igs-1010))
		(make-op :action 'take-igs-3010
			:add-list '(igs-3010))
		(make-op :action 'take-igs-3040
			:preconds '(hist-3040)
			:add-list '(igs-3040))
		(make-op :action 'take-igs-5900
			:add-list '(igs-5900))
		(make-op :action 'take-igs-5905
			:preconds '(igs-1010)
			:add-list '(igs-5905))
		(make-op :action 'take-igs-5910
			:add-list '(igs-5910))
		(make-op :action 'take-igs-5920
			:add-list '(igs-5920))
		(make-op :action 'take-igs-5930
			:add-list '(igs-5930))
		(make-op :action 'take-ital-1010
			:add-list '(ital-1010))
		(make-op :action 'take-ital-1020
			:preconds '(ital-1010)
			:add-list '(ital-1020))
		(make-op :action 'take-ital-2010
			:preconds '(ital-1020)
			:add-list '(ital-2010))
		(make-op :action 'take-ital-2020
			:preconds '(ital-2010)
			:add-list '(ital-2020))
		(make-op :action 'take-ital-2988
			:add-list '(ital-2988))
		(make-op :action 'take-japn-1010
			:add-list '(japn-1010))
		(make-op :action 'take-japn-1020
			:preconds '(japn-1010)
			:add-list '(japn-1020))
		(make-op :action 'take-japn-2010
			:preconds '(japn-1020)
			:add-list '(japn-2010))
		(make-op :action 'take-japn-2020
			:preconds '(japn-2010)
			:add-list '(japn-2020))
		(make-op :action 'take-japn-2988
			:add-list '(japn-2988))
		(make-op :action 'take-japn-3000
			:preconds '(japn-3010 japn-2020)
			:add-list '(japn-3000))
		(make-op :action 'take-japn-3010
			:preconds '(japn-2020)
			:add-list '(japn-3010))
		(make-op :action 'take-japn-3020
			:preconds '(japn-3010)
			:add-list '(japn-3020))
		(make-op :action 'take-japn-3050
			:preconds '(art-3050)
			:add-list '(japn-3050))
		(make-op :action 'take-japn-3090
			:add-list '(japn-3090))
		(make-op :action 'take-japn-3100
			:preconds '(japn-3010 japn-3020)
			:add-list '(japn-3100))
		(make-op :action 'take-japn-3510
			:preconds '(japn-3020)
			:add-list '(japn-3510))
		(make-op :action 'take-japn-3560
			:add-list '(japn-3560))
		(make-op :action 'take-japn-4250
			:preconds '(japn-3010 japn-3020 japn-3510)
			:add-list '(japn-4250))
		(make-op :action 'take-japn-4920
			:add-list '(japn-4920))
		(make-op :action 'take-jcom-1040
			:add-list '(jcom-1040))
		(make-op :action 'take-jcom-1050
			:add-list '(jcom-1050))
		(make-op :action 'take-jcom-1130
			:add-list '(jcom-1130))
		(make-op :action 'take-jcom-1500
			:add-list '(jcom-1500))
		(make-op :action 'take-jcom-1510
			:add-list '(jcom-1510))
		(make-op :action 'take-jcom-1560
			:add-list '(jcom-1560))
		(make-op :action 'take-jcom-1660
			:add-list '(jcom-1660))
		(make-op :action 'take-jcom-1720
			:add-list '(jcom-1720))
		(make-op :action 'take-jcom-1800
			:add-list '(jcom-1800))
		(make-op :action 'take-jcom-2010
			:add-list '(jcom-2010))
		(make-op :action 'take-jcom-2020
			:preconds '(stat-1040 stat-1045 stat-2000 stat-2300 jcom-1500 jcom-2010)
			:add-list '(jcom-2020))
		(make-op :action 'take-jcom-2030
			:add-list '(jcom-2030))
		(make-op :action 'take-jcom-2070
			:add-list '(jcom-2070))
		(make-op :action 'take-jcom-2080
			:add-list '(jcom-2080))
		(make-op :action 'take-jcom-2160
			:preconds '(jcom-1130 jcom-1500 jcom-2010)
			:add-list '(jcom-2160))
		(make-op :action 'take-jcom-2180
			:add-list '(jcom-2180))
		(make-op :action 'take-jcom-2220
			:add-list '(jcom-2220))
		(make-op :action 'take-jcom-2300
			:add-list '(jcom-2300))
		(make-op :action 'take-jcom-2400
			:add-list '(jcom-2400))
		(make-op :action 'take-jcom-2560
			:add-list '(jcom-2560))
		(make-op :action 'take-jcom-2660
			:add-list '(jcom-2660))
		(make-op :action 'take-jcom-2977
			:add-list '(jcom-2977))
		(make-op :action 'take-jcom-2988
			:add-list '(jcom-2988))
		(make-op :action 'take-jcom-3030
			:add-list '(jcom-3030))
		(make-op :action 'take-jcom-3050
			:preconds '(aste-3050)
			:add-list '(jcom-3050))
		(make-op :action 'take-jcom-3090
			:preconds '(aste-3090)
			:add-list '(jcom-3090))
		(make-op :action 'take-jcom-3100
			:preconds '(jcom-1130 jcom-1500 jcom-2010)
			:add-list '(jcom-3100))
		(make-op :action 'take-jcom-3110
			:add-list '(jcom-3110))
		(make-op :action 'take-jcom-3120
			:preconds '(jcom-2020 jcom-3100 jcom-3200 jcom-3310)
			:add-list '(jcom-3120))
		(make-op :action 'take-jcom-3140
			:add-list '(jcom-3140))
		(make-op :action 'take-jcom-3200
			:preconds '(jcom-1130 jcom-1500 jcom-2010)
			:add-list '(jcom-3200))
		(make-op :action 'take-jcom-3310
			:preconds '(jcom-1130 jcom-1500 jcom-2010 jcom-2300)
			:add-list '(jcom-3310))
		(make-op :action 'take-jcom-3320
			:preconds '(jcom-2020 jcom-2300 jcom-3310)
			:add-list '(jcom-3320))
		(make-op :action 'take-jcom-3410
			:add-list '(jcom-3410))
		(make-op :action 'take-jcom-4010
			:preconds '(jcom-6440)
			:add-list '(jcom-4010))
		(make-op :action 'take-jcom-4020
			:add-list '(jcom-4020))
		(make-op :action 'take-jcom-4030
			:preconds '(jcom-6430)
			:add-list '(jcom-4030))
		(make-op :action 'take-jcom-4040
			:preconds '(jcom-2030)
			:add-list '(jcom-4040))
		(make-op :action 'take-jcom-4100
			:preconds '(jcom-3110)
			:add-list '(jcom-4100))
		(make-op :action 'take-jcom-4110
			:preconds '(jcom-3100 jcom-3200 jcom-3310)
			:add-list '(jcom-4110))
		(make-op :action 'take-jcom-4120
			:add-list '(jcom-4120))
		(make-op :action 'take-jcom-4130
			:add-list '(jcom-4130))
		(make-op :action 'take-jcom-4150
			:preconds '(jcom-2180)
			:add-list '(jcom-4150))
		(make-op :action 'take-jcom-4210
			:preconds '(jcom-2020 jcom-2220 jcom-3200)
			:add-list '(jcom-4210))
		(make-op :action 'take-jcom-4220
			:preconds '(jcom-3200 jcom-4210)
			:add-list '(jcom-4220))
		(make-op :action 'take-jcom-4230
			:preconds '(jcom-1130 jcom-1500 jcom-2010 jcom-2020 jcom-2220)
			:add-list '(jcom-4230))
		(make-op :action 'take-jcom-4300
			:add-list '(jcom-4300))
		(make-op :action 'take-jcom-4340
			:preconds '(jcom-1130 jcom-2300)
			:add-list '(jcom-4340))
		(make-op :action 'take-jcom-4400
			:preconds '(jcom-6400 jcom-6400)
			:add-list '(jcom-4400))
		(make-op :action 'take-jcom-4410
			:preconds '(jcom-6410)
			:add-list '(jcom-4410))
		(make-op :action 'take-jcom-4500
			:add-list '(jcom-4500))
		(make-op :action 'take-jcom-4510
			:add-list '(jcom-4510))
		(make-op :action 'take-jcom-4520
			:preconds '(honr-4800)
			:add-list '(jcom-4520))
		(make-op :action 'take-jcom-4530
			:add-list '(jcom-4530))
		(make-op :action 'take-jcom-4750
			:add-list '(jcom-4750))
		(make-op :action 'take-jcom-4760
			:add-list '(jcom-4760))
		(make-op :action 'take-jcom-4770
			:add-list '(jcom-4770))
		(make-op :action 'take-jcom-5010
			:preconds '(jcom-6010)
			:add-list '(jcom-5010))
		(make-op :action 'take-jcom-5020
			:preconds '(jcom-6020)
			:add-list '(jcom-5020))
		(make-op :action 'take-jcom-5030
			:preconds '(jcom-6030)
			:add-list '(jcom-5030))
		(make-op :action 'take-jcom-5040
			:add-list '(jcom-5040))
		(make-op :action 'take-jcom-5110
			:preconds '(jcom-6110)
			:add-list '(jcom-5110))
		(make-op :action 'take-jcom-5210
			:preconds '(jcom-6210)
			:add-list '(jcom-5210))
		(make-op :action 'take-jcom-5220
			:preconds '(jcom-4220 jcom-4230 jcom-6220)
			:add-list '(jcom-5220))
		(make-op :action 'take-jcom-5230
			:preconds '(jcom-2220 jcom-2030 jcom-6230)
			:add-list '(jcom-5230))
		(make-op :action 'take-jcom-5300
			:preconds '(jcom-3320 jcom-6300)
			:add-list '(jcom-5300))
		(make-op :action 'take-jcom-5310
			:preconds '(jcom-6310)
			:add-list '(jcom-5310))
		(make-op :action 'take-jcom-5320
			:preconds '(jcom-6320)
			:add-list '(jcom-5320))
		(make-op :action 'take-jcom-5400
			:preconds '(jcom-2020 jcom-4040)
			:add-list '(jcom-5400))
		(make-op :action 'take-jcom-5410
			:preconds '(jcom-6060)
			:add-list '(jcom-5410))
		(make-op :action 'take-jcom-5420
			:preconds '(pols-5420 jcom-6420)
			:add-list '(jcom-5420))
		(make-op :action 'take-kin-1900
			:add-list '(kin-1900))
		(make-op :action 'take-kin-2000
			:add-list '(kin-2000))
		(make-op :action 'take-kin-2020
			:add-list '(kin-2020))
		(make-op :action 'take-kin-2030
			:add-list '(kin-2030))
		(make-op :action 'take-kin-2050
			:add-list '(kin-2050))
		(make-op :action 'take-kin-2100
			:add-list '(kin-2100))
		(make-op :action 'take-kin-2200
			:add-list '(kin-2200))
		(make-op :action 'take-kin-2300
			:add-list '(kin-2300))
		(make-op :action 'take-kin-2400
			:add-list '(kin-2400))
		(make-op :action 'take-kin-2500
			:add-list '(kin-2500))
		(make-op :action 'take-kin-2550
			:add-list '(kin-2550))
		(make-op :action 'take-kin-2650
			:add-list '(kin-2650))
		(make-op :action 'take-kin-2710
			:add-list '(kin-2710))
		(make-op :action 'take-kin-2977
			:add-list '(kin-2977))
		(make-op :action 'take-kin-3000
			:add-list '(kin-3000))
		(make-op :action 'take-kin-3010
			:add-list '(kin-3010))
		(make-op :action 'take-kin-3011
			:add-list '(kin-3011))
		(make-op :action 'take-kin-3020
			:add-list '(kin-3020))
		(make-op :action 'take-kin-3021
			:preconds '(kin-3010)
			:add-list '(kin-3021))
		(make-op :action 'take-kin-3050
			:add-list '(kin-3050))
		(make-op :action 'take-kin-3070
			:add-list '(kin-3070))
		(make-op :action 'take-kin-3100
			:add-list '(kin-3100))
		(make-op :action 'take-kin-3200
			:add-list '(kin-3200))
		(make-op :action 'take-kin-3250
			:add-list '(kin-3250))
		(make-op :action 'take-kin-3300
			:add-list '(kin-3300))
		(make-op :action 'take-kin-3500
			:add-list '(kin-3500))
		(make-op :action 'take-kin-3550
			:add-list '(kin-3550))
		(make-op :action 'take-kin-3600
			:preconds '(kin-3050)
			:add-list '(kin-3600))
		(make-op :action 'take-kin-3650
			:add-list '(kin-3650))
		(make-op :action 'take-kin-3830
			:preconds '(engl-3830 hist-3830)
			:add-list '(kin-3830))
		(make-op :action 'take-kin-4000
			:preconds '(psy-4000)
			:add-list '(kin-4000))
		(make-op :action 'take-kin-4015
			:add-list '(kin-4015))
		(make-op :action 'take-kin-4100
			:preconds '(biol-2320 biol-2420 math-1050 stat-1040 stat-1045)
			:add-list '(kin-4100))
		(make-op :action 'take-kin-4150
			:preconds '(kin-3100)
			:add-list '(kin-4150))
		(make-op :action 'take-kin-4200
			:preconds '(biol-2320 biol-2420 kin-3250 math-1050 stat-1040 stat-1045)
			:add-list '(kin-4200))
		(make-op :action 'take-kin-4250
			:add-list '(kin-4250))
		(make-op :action 'take-kin-4300
			:add-list '(kin-4300))
		(make-op :action 'take-kin-4350
			:add-list '(kin-4350))
		(make-op :action 'take-kin-4400
			:preconds '(kin-2000 kin-2020)
			:add-list '(kin-4400))
		(make-op :action 'take-kin-4500
			:add-list '(kin-4500))
		(make-op :action 'take-kin-4570
			:preconds '(biol-2320 biol-2420 kin-3100 kin-3250)
			:add-list '(kin-4570))
		(make-op :action 'take-kin-4580
			:preconds '(biol-2320 biol-2420 kin-3100 kin-3250)
			:add-list '(kin-4580))
		(make-op :action 'take-kin-4600
			:preconds '(kin-4500)
			:add-list '(kin-4600))
		(make-op :action 'take-kin-4700
			:preconds '(kin-4500)
			:add-list '(kin-4700))
		(make-op :action 'take-kin-4800
			:preconds '(kin-4500)
			:add-list '(kin-4800))
		(make-op :action 'take-kin-4850
			:add-list '(kin-4850))
		(make-op :action 'take-kin-4900
			:preconds '(kin-2000 kin-3300 kin-4300)
			:add-list '(kin-4900))
		(make-op :action 'take-kin-4950
			:add-list '(kin-4950))
		(make-op :action 'take-kin-5050
			:preconds '(psy-5050 kin-6050)
			:add-list '(kin-5050))
		(make-op :action 'take-kin-5070
			:add-list '(kin-5070))
		(make-op :action 'take-kin-5100
			:preconds '(kin-4100)
			:add-list '(kin-5100))
		(make-op :action 'take-kin-5150
			:preconds '(biol-2320 biol-2420)
			:add-list '(kin-5150))
		(make-op :action 'take-kin-5200
			:add-list '(kin-5200))
		(make-op :action 'take-kin-5430
			:add-list '(kin-5430))
		(make-op :action 'take-kin-5500
			:preconds '(kin-4900)
			:add-list '(kin-5500))
		(make-op :action 'take-kin-5560
			:add-list '(kin-5560))
		(make-op :action 'take-kin-5630
			:preconds '(kin-4900)
			:add-list '(kin-5630))
		(make-op :action 'take-kin-5700
			:preconds '(kin-6700)
			:add-list '(kin-5700))
		(make-op :action 'take-kin-5900
			:add-list '(kin-5900))
		(make-op :action 'take-kin-5910
			:add-list '(kin-5910))
		(make-op :action 'take-kor-1010
			:add-list '(kor-1010))
		(make-op :action 'take-kor-1020
			:preconds '(kor-1010)
			:add-list '(kor-1020))
		(make-op :action 'take-kor-2010
			:preconds '(kor-1020)
			:add-list '(kor-2010))
		(make-op :action 'take-kor-2020
			:preconds '(kor-2010)
			:add-list '(kor-2020))
		(make-op :action 'take-kor-3010
			:preconds '(kor-2020)
			:add-list '(kor-3010))
		(make-op :action 'take-kor-3020
			:preconds '(kor-3010)
			:add-list '(kor-3020))
		(make-op :action 'take-kor-3510
			:preconds '(kor-2010)
			:add-list '(kor-3510))
		(make-op :action 'take-kor-4920
			:add-list '(kor-4920))
		(make-op :action 'take-laep-1030
			:add-list '(laep-1030))
		(make-op :action 'take-laep-1040
			:add-list '(laep-1040))
		(make-op :action 'take-laep-1200
			:add-list '(laep-1200))
		(make-op :action 'take-laep-1300
			:add-list '(laep-1300))
		(make-op :action 'take-laep-1350
			:preconds '(laep-1200)
			:add-list '(laep-1350))
		(make-op :action 'take-laep-2039
			:add-list '(laep-2039))
		(make-op :action 'take-laep-2250
			:add-list '(laep-2250))
		(make-op :action 'take-laep-2300
			:add-list '(laep-2300))
		(make-op :action 'take-laep-2400
			:preconds '(laep-1300)
			:add-list '(laep-2400))
		(make-op :action 'take-laep-2500
			:add-list '(laep-2500))
		(make-op :action 'take-laep-2600
			:preconds '(laep-1200)
			:add-list '(laep-2600))
		(make-op :action 'take-laep-2700
			:add-list '(laep-2700))
		(make-op :action 'take-laep-2720
			:preconds '(laep-1350 laep-2700)
			:add-list '(laep-2720))
		(make-op :action 'take-laep-2900
			:add-list '(laep-2900))
		(make-op :action 'take-laep-3100
			:add-list '(laep-3100))
		(make-op :action 'take-laep-3120
			:add-list '(laep-3120))
		(make-op :action 'take-laep-3300
			:add-list '(laep-3300))
		(make-op :action 'take-laep-3400
			:preconds '(laep-3300)
			:add-list '(laep-3400))
		(make-op :action 'take-laep-3500
			:add-list '(laep-3500))
		(make-op :action 'take-laep-3610
			:add-list '(laep-3610))
		(make-op :action 'take-laep-3700
			:add-list '(laep-3700))
		(make-op :action 'take-laep-4040
			:preconds '(laep-6060)
			:add-list '(laep-4040))
		(make-op :action 'take-laep-4100
			:add-list '(laep-4100))
		(make-op :action 'take-laep-4110
			:add-list '(laep-4110))
		(make-op :action 'take-laep-4120
			:add-list '(laep-4120))
		(make-op :action 'take-laep-4200
			:preconds '(laep-6200)
			:add-list '(laep-4200))
		(make-op :action 'take-laep-4210
			:preconds '(laep-6210)
			:add-list '(laep-4210))
		(make-op :action 'take-laep-4250
			:add-list '(laep-4250))
		(make-op :action 'take-laep-4350
			:preconds '(laep-6550)
			:add-list '(laep-4350))
		(make-op :action 'take-laep-4700
			:add-list '(laep-4700))
		(make-op :action 'take-laep-4900
			:add-list '(laep-4900))
		(make-op :action 'take-laep-4910
			:preconds '(laep-6160)
			:add-list '(laep-4910))
		(make-op :action 'take-laep-4911
			:preconds '(laep-4910 laep-6160 laep-6911)
			:add-list '(laep-4911))
		(make-op :action 'take-laep-5090
			:preconds '(psc-5090 laep-6090)
			:add-list '(laep-5090))
		(make-op :action 'take-lang-1050
			:add-list '(lang-1050))
		(make-op :action 'take-lang-2100
			:preconds '(ling-2100)
			:add-list '(lang-2100))
		(make-op :action 'take-lang-2420
			:preconds '(ieli-2420)
			:add-list '(lang-2420))
		(make-op :action 'take-lang-3100
			:add-list '(lang-3100))
		(make-op :action 'take-lang-3110
			:add-list '(lang-3110))
		(make-op :action 'take-lang-3120
			:add-list '(lang-3120))
		(make-op :action 'take-lang-3150
			:preconds '(lang-3100 lang-3110)
			:add-list '(lang-3150))
		(make-op :action 'take-lang-3550
			:add-list '(lang-3550))
		(make-op :action 'take-lang-3570
			:add-list '(lang-3570))
		(make-op :action 'take-lang-3990
			:add-list '(lang-3990))
		(make-op :action 'take-lang-4200
			:add-list '(lang-4200))
		(make-op :action 'take-lang-4210
			:add-list '(lang-4210))
		(make-op :action 'take-lang-4330
			:add-list '(lang-4330))
		(make-op :action 'take-lang-4822
			:add-list '(lang-4822))
		(make-op :action 'take-lang-5950
			:preconds '(cmst-1330 cmst-3330)
			:add-list '(lang-5950))
		(make-op :action 'take-latn-1010
			:add-list '(latn-1010))
		(make-op :action 'take-latn-1020
			:preconds '(latn-1010)
			:add-list '(latn-1020))
		(make-op :action 'take-latn-3100
			:preconds '(latn-1020)
			:add-list '(latn-3100))
		(make-op :action 'take-latn-3130
			:preconds '(latn-3100)
			:add-list '(latn-3130))
		(make-op :action 'take-latn-4100
			:preconds '(latn-3100 latn-3130)
			:add-list '(latn-4100))
		(make-op :action 'take-latn-4860
			:preconds '(latn-3100 latn-3130)
			:add-list '(latn-4860))
		(make-op :action 'take-latn-4930
			:add-list '(latn-4930))
		(make-op :action 'take-lats-2200
			:add-list '(lats-2200))
		(make-op :action 'take-ling-2100
			:preconds '(lang-2100)
			:add-list '(ling-2100))
		(make-op :action 'take-ling-2250
			:add-list '(ling-2250))
		(make-op :action 'take-ling-2500
			:add-list '(ling-2500))
		(make-op :action 'take-ling-3100
			:add-list '(ling-3100))
		(make-op :action 'take-ling-3200
			:add-list '(ling-3200))
		(make-op :action 'take-ling-3300
			:preconds '(ling-5400 ling-6400)
			:add-list '(ling-3300))
		(make-op :action 'take-ling-4100
			:preconds '(anth-4100)
			:add-list '(ling-4100))
		(make-op :action 'take-ling-4250
			:add-list '(ling-4250))
		(make-op :action 'take-ling-4300
			:preconds '(ling-5400 ling-6400)
			:add-list '(ling-4300))
		(make-op :action 'take-ling-4520
			:preconds '(ling-6520)
			:add-list '(ling-4520))
		(make-op :action 'take-ling-4700
			:preconds '(ling-6700)
			:add-list '(ling-4700))
		(make-op :action 'take-ling-4900
			:add-list '(ling-4900))
		(make-op :action 'take-ling-4920
			:add-list '(ling-4920))
		(make-op :action 'take-ling-5400
			:preconds '(ling-3300 ling-4300)
			:add-list '(ling-5400))
		(make-op :action 'take-ling-5410
			:preconds '(ling-5400)
			:add-list '(ling-5410))
		(make-op :action 'take-ling-5500
			:add-list '(ling-5500))
		(make-op :action 'take-ling-5630
			:add-list '(ling-5630))
		(make-op :action 'take-mach-1010
			:add-list '(mach-1010))
		(make-op :action 'take-mach-1020
			:add-list '(mach-1020))
		(make-op :action 'take-mach-1030
			:add-list '(mach-1030))
		(make-op :action 'take-mach-1040
			:add-list '(mach-1040))
		(make-op :action 'take-mach-1050
			:add-list '(mach-1050))
		(make-op :action 'take-mach-1140
			:add-list '(mach-1140))
		(make-op :action 'take-mach-1160
			:add-list '(mach-1160))
		(make-op :action 'take-mach-1200
			:add-list '(mach-1200))
		(make-op :action 'take-mach-2977
			:add-list '(mach-2977))
		(make-op :action 'take-mach-2988
			:add-list '(mach-2988))
		(make-op :action 'take-mgt-1000
			:add-list '(mgt-1000))
		(make-op :action 'take-mgt-3050
			:add-list '(mgt-3050))
		(make-op :action 'take-mgt-3100
			:add-list '(mgt-3100))
		(make-op :action 'take-mgt-3150
			:add-list '(mgt-3150))
		(make-op :action 'take-mgt-3250
			:add-list '(mgt-3250))
		(make-op :action 'take-mgt-3350
			:add-list '(mgt-3350))
		(make-op :action 'take-mgt-3400
			:add-list '(mgt-3400))
		(make-op :action 'take-mgt-3600
			:preconds '(data-3100 stat-2000 stat-2300 stat-3000)
			:add-list '(mgt-3600))
		(make-op :action 'take-mgt-3700
			:preconds '(data-3100 psy-3010 stat-2000 stat-2300 stat-3000)
			:add-list '(mgt-3700))
		(make-op :action 'take-mgt-3710
			:preconds '(msle-3710)
			:add-list '(mgt-3710))
		(make-op :action 'take-mgt-3810
			:add-list '(mgt-3810))
		(make-op :action 'take-mgt-3830
			:add-list '(mgt-3830))
		(make-op :action 'take-mgt-3940
			:add-list '(mgt-3940))
		(make-op :action 'take-mgt-3950
			:preconds '(mgt-3940)
			:add-list '(mgt-3950))
		(make-op :action 'take-mgt-3960
			:preconds '(mgt-3940)
			:add-list '(mgt-3960))
		(make-op :action 'take-mgt-3970
			:preconds '(mgt-3940)
			:add-list '(mgt-3970))
		(make-op :action 'take-mgt-4250
			:add-list '(mgt-4250))
		(make-op :action 'take-mgt-4500
			:add-list '(mgt-4500))
		(make-op :action 'take-mgt-4510
			:add-list '(mgt-4510))
		(make-op :action 'take-mgt-4520
			:add-list '(mgt-4520))
		(make-op :action 'take-mgt-4600
			:add-list '(mgt-4600))
		(make-op :action 'take-mgt-4610
			:preconds '(mgt-3250)
			:add-list '(mgt-4610))
		(make-op :action 'take-mgt-4620
			:preconds '(mgt-3250)
			:add-list '(mgt-4620))
		(make-op :action 'take-mgt-4630
			:preconds '(mgt-3250)
			:add-list '(mgt-4630))
		(make-op :action 'take-mgt-4720
			:preconds '(mgt-3700)
			:add-list '(mgt-4720))
		(make-op :action 'take-mgt-4740
			:preconds '(mgt-3700)
			:add-list '(mgt-4740))
		(make-op :action 'take-mgt-4760
			:preconds '(mgt-3700)
			:add-list '(mgt-4760))
		(make-op :action 'take-mgt-4790
			:preconds '(mgt-3700)
			:add-list '(mgt-4790))
		(make-op :action 'take-mgt-4800
			:add-list '(mgt-4800))
		(make-op :action 'take-mgt-4850
			:add-list '(mgt-4850))
		(make-op :action 'take-mgt-4950
			:add-list '(mgt-4950))
		(make-op :action 'take-mgt-5730
			:preconds '(mgt-3700)
			:add-list '(mgt-5730))
		(make-op :action 'take-msle-2500
			:add-list '(msle-2500))
		(make-op :action 'take-msle-2600
			:add-list '(msle-2600))
		(make-op :action 'take-msle-2650
			:add-list '(msle-2650))
		(make-op :action 'take-msle-2700
			:add-list '(msle-2700))
		(make-op :action 'take-msle-2750
			:add-list '(msle-2750))
		(make-op :action 'take-msle-2800
			:add-list '(msle-2800))
		(make-op :action 'take-msle-2900
			:add-list '(msle-2900))
		(make-op :action 'take-msle-3110
			:add-list '(msle-3110))
		(make-op :action 'take-msle-3300
			:preconds '(ecn-3300)
			:add-list '(msle-3300))
		(make-op :action 'take-msle-3500
			:add-list '(msle-3500))
		(make-op :action 'take-msle-3510
			:add-list '(msle-3510))
		(make-op :action 'take-msle-3530
			:add-list '(msle-3530))
		(make-op :action 'take-msle-3540
			:add-list '(msle-3540))
		(make-op :action 'take-msle-3550
			:add-list '(msle-3550))
		(make-op :action 'take-msle-3580
			:add-list '(msle-3580))
		(make-op :action 'take-msle-3590
			:add-list '(msle-3590))
		(make-op :action 'take-msle-3710
			:add-list '(msle-3710))
		(make-op :action 'take-msle-3800
			:add-list '(msle-3800))
		(make-op :action 'take-msle-3810
			:add-list '(msle-3810))
		(make-op :action 'take-msle-3820
			:add-list '(msle-3820))
		(make-op :action 'take-msle-3830
			:add-list '(msle-3830))
		(make-op :action 'take-msle-3840
			:add-list '(msle-3840))
		(make-op :action 'take-msle-3850
			:preconds '(msle-3500)
			:add-list '(msle-3850))
		(make-op :action 'take-msle-3855
			:add-list '(msle-3855))
		(make-op :action 'take-msle-3875
			:preconds '(msle-3500)
			:add-list '(msle-3875))
		(make-op :action 'take-msle-3880
			:add-list '(msle-3880))
		(make-op :action 'take-msle-3890
			:preconds '(acct-2020 msle-3500 fin-3400 fin-3300)
			:add-list '(msle-3890))
		(make-op :action 'take-msle-4000
			:add-list '(msle-4000))
		(make-op :action 'take-msle-4050
			:preconds '(msle-3500)
			:add-list '(msle-4050))
		(make-op :action 'take-msle-4200
			:preconds '(mgt-3100 mgt-3600 msle-4532)
			:add-list '(msle-4200))
		(make-op :action 'take-msle-4250
			:preconds '(msle-3500 msle-3800 mgt-3700)
			:add-list '(msle-4250))
		(make-op :action 'take-msle-4510
			:preconds '(msle-3500)
			:add-list '(msle-4510))
		(make-op :action 'take-msle-4520
			:preconds '(msle-3500)
			:add-list '(msle-4520))
		(make-op :action 'take-msle-4525
			:add-list '(msle-4525))
		(make-op :action 'take-msle-4531
			:preconds '(msle-3500 stat-1040 stat-1045 stat-2000 stat-2300 stat-3000 psy-3010 data-3100)
			:add-list '(msle-4531))
		(make-op :action 'take-msle-4532
			:preconds '(msle-3500 stat-1040 stat-1045 stat-2000 stat-2300 stat-3000 psy-3010 data-3100)
			:add-list '(msle-4532))
		(make-op :action 'take-msle-4533
			:preconds '(msle-3500 stat-1040 stat-1045 stat-2000 stat-2300 stat-3000 psy-3010 data-3100)
			:add-list '(msle-4533))
		(make-op :action 'take-msle-4534
			:preconds '(msle-3500 stat-1040 stat-1045 stat-2000 stat-2300 stat-3000 psy-3010 data-3100)
			:add-list '(msle-4534))
		(make-op :action 'take-msle-4535
			:preconds '(msle-3500)
			:add-list '(msle-4535))
		(make-op :action 'take-msle-4536
			:add-list '(msle-4536))
		(make-op :action 'take-msle-4540
			:preconds '(msle-3500)
			:add-list '(msle-4540))
		(make-op :action 'take-msle-4541
			:add-list '(msle-4541))
		(make-op :action 'take-msle-4545
			:preconds '(msle-3500)
			:add-list '(msle-4545))
		(make-op :action 'take-msle-4550
			:preconds '(msle-3500)
			:add-list '(msle-4550))
		(make-op :action 'take-msle-4555
			:preconds '(msle-3500 stat-1040 stat-1045 stat-2000 stat-2300 stat-3000 psy-3010 data-3100)
			:add-list '(msle-4555))
		(make-op :action 'take-msle-4560
			:add-list '(msle-4560))
		(make-op :action 'take-msle-4562
			:preconds '(msle-3500 msle-4560)
			:add-list '(msle-4562))
		(make-op :action 'take-msle-4565
			:preconds '(msle-3500)
			:add-list '(msle-4565))
		(make-op :action 'take-msle-4570
			:preconds '(msle-3500 psy-3010 stat-1040 stat-1045 stat-2000 stat-2300 stat-3000 data-3100)
			:add-list '(msle-4570))
		(make-op :action 'take-msle-4575
			:preconds '(msle-4560)
			:add-list '(msle-4575))
		(make-op :action 'take-msle-4580
			:preconds '(msle-3500)
			:add-list '(msle-4580))
		(make-op :action 'take-msle-4590
			:preconds '(msle-3500)
			:add-list '(msle-4590))
		(make-op :action 'take-msle-4800
			:add-list '(msle-4800))
		(make-op :action 'take-msle-4890
			:preconds '(fin-3400 mgt-3700 msle-3500 msle-3890)
			:add-list '(msle-4890))
		(make-op :action 'take-msle-4895
			:preconds '(mgt-3600 msle-4532)
			:add-list '(msle-4895))
		(make-op :action 'take-msle-4950
			:add-list '(msle-4950))
		(make-op :action 'take-math-1020
			:add-list '(math-1020))
		(make-op :action 'take-math-1030
			:add-list '(math-1030))
		(make-op :action 'take-math-1050
			:add-list '(math-1050))
		(make-op :action 'take-math-1051
			:add-list '(math-1051))
		(make-op :action 'take-math-1060
			:add-list '(math-1060))
		(make-op :action 'take-math-1100
			:preconds '(math-1050)
			:add-list '(math-1100))
		(make-op :action 'take-math-1210
			:preconds '(math-1050 math-1060)
			:add-list '(math-1210))
		(make-op :action 'take-math-1220
			:preconds '(math-1210)
			:add-list '(math-1220))
		(make-op :action 'take-math-2010
			:preconds '(math-1050 math-1051)
			:add-list '(math-2010))
		(make-op :action 'take-math-2020
			:preconds '(math-1050 math-1051 math-2010)
			:add-list '(math-2020))
		(make-op :action 'take-math-2210
			:preconds '(math-1220)
			:add-list '(math-2210))
		(make-op :action 'take-math-2250
			:preconds '(math-1220)
			:add-list '(math-2250))
		(make-op :action 'take-math-2260
			:add-list '(math-2260))
		(make-op :action 'take-math-2270
			:preconds '(math-1210)
			:add-list '(math-2270))
		(make-op :action 'take-math-2280
			:preconds '(math-1220 math-2270)
			:add-list '(math-2280))
		(make-op :action 'take-math-2910
			:add-list '(math-2910))
		(make-op :action 'take-math-2977
			:add-list '(math-2977))
		(make-op :action 'take-math-3010
			:preconds '(math-2250 math-2270 math-1210 math-2010 math-2020)
			:add-list '(math-3010))
		(make-op :action 'take-math-3110
			:preconds '(math-1220 math-1210 math-2020)
			:add-list '(math-3110))
		(make-op :action 'take-math-3300
			:add-list '(math-3300))
		(make-op :action 'take-math-3310
			:preconds '(math-1210 math-1220)
			:add-list '(math-3310))
		(make-op :action 'take-math-4200
			:preconds '(math-2210 math-3310 math-2250 math-2270 math-2280)
			:add-list '(math-4200))
		(make-op :action 'take-math-4230
			:preconds '(math-2280 math-2270 math-2250 biol-4230)
			:add-list '(math-4230))
		(make-op :action 'take-math-4250
			:add-list '(math-4250))
		(make-op :action 'take-math-4300
			:add-list '(math-4300))
		(make-op :action 'take-math-4310
			:preconds '(math-2210 math-3310 math-2270 math-2280 math-2250)
			:add-list '(math-4310))
		(make-op :action 'take-math-4400
			:preconds '(math-4200 math-4310 math-3110 math-3110)
			:add-list '(math-4400))
		(make-op :action 'take-math-4410
			:preconds '(math-2270 math-3310)
			:add-list '(math-4410))
		(make-op :action 'take-math-4500
			:preconds '(math-6500 math-6500 math-3110 math-4200 math-4310 math-6500)
			:add-list '(math-4500))
		(make-op :action 'take-math-4610
			:preconds '(math-1220 math-2250 math-2270)
			:add-list '(math-4610))
		(make-op :action 'take-math-4700
			:preconds '(math-2210 math-2250 math-2280)
			:add-list '(math-4700))
		(make-op :action 'take-math-4910
			:add-list '(math-4910))
		(make-op :action 'take-math-5010
			:preconds '(math-3110)
			:add-list '(math-5010))
		(make-op :action 'take-math-5020
			:add-list '(math-5020))
		(make-op :action 'take-math-5110
			:preconds '(math-2210 math-2270 math-4200 math-4310 math-5340)
			:add-list '(math-5110))
		(make-op :action 'take-math-5210
			:preconds '(math-4200 math-5510)
			:add-list '(math-5210))
		(make-op :action 'take-math-5220
			:preconds '(math-5210 math-5210)
			:add-list '(math-5220))
		(make-op :action 'take-math-5270
			:preconds '(math-2210 math-2250 math-2210 math-2270 math-2280)
			:add-list '(math-5270))
		(make-op :action 'take-math-5310
			:preconds '(math-4310 math-4310)
			:add-list '(math-5310))
		(make-op :action 'take-math-5340
			:preconds '(math-2270 math-4310)
			:add-list '(math-5340))
		(make-op :action 'take-math-5410
			:add-list '(math-5410))
		(make-op :action 'take-math-5420
			:preconds '(math-2250 math-2280)
			:add-list '(math-5420))
		(make-op :action 'take-math-5460
			:preconds '(math-2250 math-2280)
			:add-list '(math-5460))
		(make-op :action 'take-math-5470
			:preconds '(math-2250 math-2270 math-2280)
			:add-list '(math-5470))
		(make-op :action 'take-math-5510
			:preconds '(math-4200)
			:add-list '(math-5510))
		(make-op :action 'take-math-5560
			:preconds '(math-1220 stat-3000)
			:add-list '(math-5560))
		(make-op :action 'take-math-5570
			:preconds '(math-5710 stat-3000)
			:add-list '(math-5570))
		(make-op :action 'take-math-5580
			:preconds '(math-5570 math-5570)
			:add-list '(math-5580))
		(make-op :action 'take-math-5610
			:preconds '(math-4610)
			:add-list '(math-5610))
		(make-op :action 'take-math-5620
			:preconds '(math-4610 math-2250 math-2280)
			:add-list '(math-5620))
		(make-op :action 'take-math-5645
			:preconds '(math-1210 stat-3000 math-5710 math-1220 math-2270 stat-5645 math-6645)
			:add-list '(math-5645))
		(make-op :action 'take-math-5710
			:preconds '(math-1220)
			:add-list '(math-5710))
		(make-op :action 'take-math-5720
			:preconds '(math-5710)
			:add-list '(math-5720))
		(make-op :action 'take-math-5760
			:preconds '(math-5710)
			:add-list '(math-5760))
		(make-op :action 'take-math-5810
			:add-list '(math-5810))
		(make-op :action 'take-math-5820
			:add-list '(math-5820))
		(make-op :action 'take-math-5910
			:add-list '(math-5910))
		(make-op :action 'take-math-5950
			:add-list '(math-5950))
		(make-op :action 'take-mae-1010
			:preconds '(math-1210)
			:add-list '(mae-1010))
		(make-op :action 'take-mae-1200
			:preconds '(math-1060)
			:add-list '(mae-1200))
		(make-op :action 'take-mae-2160
			:preconds '(chem-1210 math-1220)
			:add-list '(mae-2160))
		(make-op :action 'take-mae-2165
			:preconds '(mae-2160)
			:add-list '(mae-2165))
		(make-op :action 'take-mae-2300
			:preconds '(math-1220 math-2210)
			:add-list '(mae-2300))
		(make-op :action 'take-mae-2450
			:preconds '(cs-1400 ece-1400 math-2210 math-2250 math-2270 math-2280)
			:add-list '(mae-2450))
		(make-op :action 'take-mae-3040
			:preconds '(engr-2140 math-2250 math-2270 math-2280 mae-2160 cee-3160)
			:add-list '(mae-3040))
		(make-op :action 'take-mae-3340
			:preconds '(engr-2140 engr-2210 ece-2250)
			:add-list '(mae-3340))
		(make-op :action 'take-mae-3420
			:preconds '(engr-2030 mae-2300 cs-1400 ece-1400 math-2250 math-2270 math-2280)
			:add-list '(mae-3420))
		(make-op :action 'take-mae-3440
			:preconds '(mae-2300 mae-3420)
			:add-list '(mae-3440))
		(make-op :action 'take-mae-3600
			:add-list '(mae-3600))
		(make-op :action 'take-mae-4300
			:preconds '(mae-3040)
			:add-list '(mae-4300))
		(make-op :action 'take-mae-4400
			:preconds '(mae-3340 mae-3440)
			:add-list '(mae-4400))
		(make-op :action 'take-mae-4800
			:preconds '(mae-3440 mae-4300 engr-3080)
			:add-list '(mae-4800))
		(make-op :action 'take-mae-4810
			:preconds '(mae-4800)
			:add-list '(mae-4810))
		(make-op :action 'take-mae-5020
			:preconds '(mae-3040)
			:add-list '(mae-5020))
		(make-op :action 'take-mae-5040
			:preconds '(mae-3040 mae-3340)
			:add-list '(mae-5040))
		(make-op :action 'take-mae-5050
			:preconds '(mae-2160 mae-2165 mae-5060)
			:add-list '(mae-5050))
		(make-op :action 'take-mae-5060
			:preconds '(cee-3020 mae-3040 cee-5060)
			:add-list '(mae-5060))
		(make-op :action 'take-mae-5300
			:preconds '(engr-2030 engr-2140 math-2250 math-2270 math-2280)
			:add-list '(mae-5300))
		(make-op :action 'take-mae-5310
			:preconds '(math-2250 math-2270 math-2280 mae-3340)
			:add-list '(mae-5310))
		(make-op :action 'take-mae-5320
			:preconds '(ece-5310 mae-5310 ece-5320)
			:add-list '(mae-5320))
		(make-op :action 'take-mae-5330
			:preconds '(ece-5310 mae-5310 ece-5330)
			:add-list '(mae-5330))
		(make-op :action 'take-mae-5340
			:preconds '(ece-1410 cs-1410 math-2250 math-2280 ece-5340)
			:add-list '(mae-5340))
		(make-op :action 'take-mae-5350
			:preconds '(engr-2030 math-2250 math-2270 math-2280)
			:add-list '(mae-5350))
		(make-op :action 'take-mae-5360
			:preconds '(engr-2030 math-2250 math-2270 math-2280)
			:add-list '(mae-5360))
		(make-op :action 'take-mae-5370
			:preconds '(mae-2450)
			:add-list '(mae-5370))
		(make-op :action 'take-mae-5420
			:preconds '(mae-2300 mae-3420)
			:add-list '(mae-5420))
		(make-op :action 'take-mae-5430
			:preconds '(mae-3440)
			:add-list '(mae-5430))
		(make-op :action 'take-mae-5440
			:preconds '(mae-2450 mae-3420)
			:add-list '(mae-5440))
		(make-op :action 'take-mae-5450
			:preconds '(mae-3440)
			:add-list '(mae-5450))
		(make-op :action 'take-mae-5500
			:preconds '(mae-2450 mae-3420)
			:add-list '(mae-5500))
		(make-op :action 'take-mae-5510
			:preconds '(mae-5500)
			:add-list '(mae-5510))
		(make-op :action 'take-mae-5520
			:preconds '(mae-5360 phys-3550 ece-2290 ece-2700)
			:add-list '(mae-5520))
		(make-op :action 'take-mae-5530
			:preconds '(ece-5230 mae-5520 ece-5240)
			:add-list '(mae-5530))
		(make-op :action 'take-mae-5540
			:preconds '(mae-5420)
			:add-list '(mae-5540))
		(make-op :action 'take-mae-5550
			:preconds '(mae-3040)
			:add-list '(mae-5550))
		(make-op :action 'take-mae-5560
			:add-list '(mae-5560))
		(make-op :action 'take-mae-5700
			:preconds '(phys-2210 phys-2310 phys-2215 phys-2220 phys-2320 phys-2225 beng-5700 ece-5700 phys-5700)
			:add-list '(mae-5700))
		(make-op :action 'take-mae-5900
			:add-list '(mae-5900))
		(make-op :action 'take-mae-5930
			:add-list '(mae-5930))
		(make-op :action 'take-mlt-1010
			:preconds '(chem-1010 chem-1015 engl-1010 stat-1040 stat-1045)
			:add-list '(mlt-1010))
		(make-op :action 'take-mlt-2230
			:preconds '(chem-1010 chem-1015 engl-1010 stat-1040 stat-1045)
			:add-list '(mlt-2230))
		(make-op :action 'take-mlt-2240
			:preconds '(chem-1010 chem-1015 engl-1010 stat-1040 stat-1045)
			:add-list '(mlt-2240))
		(make-op :action 'take-mlt-2270
			:preconds '(chem-1010 chem-1015 engl-1010 stat-1040 stat-1045)
			:add-list '(mlt-2270))
		(make-op :action 'take-mlt-2280
			:add-list '(mlt-2280))
		(make-op :action 'take-mlt-2430
			:preconds '(chem-1010 chem-1015 engl-1010 stat-1040 stat-1045)
			:add-list '(mlt-2430))
		(make-op :action 'take-mlt-2450
			:add-list '(mlt-2450))
		(make-op :action 'take-mlt-2550
			:add-list '(mlt-2550))
		(make-op :action 'take-mlt-2570
			:preconds '(chem-1010 chem-1015 engl-1010 stat-1040 stat-1045)
			:add-list '(mlt-2570))
		(make-op :action 'take-mlt-2580
			:preconds '(chem-1010 chem-1015 engl-1010 stat-1040 stat-1045)
			:add-list '(mlt-2580))
		(make-op :action 'take-msl-1010
			:add-list '(msl-1010))
		(make-op :action 'take-msl-1015
			:add-list '(msl-1015))
		(make-op :action 'take-msl-1020
			:add-list '(msl-1020))
		(make-op :action 'take-msl-1025
			:add-list '(msl-1025))
		(make-op :action 'take-msl-2010
			:add-list '(msl-2010))
		(make-op :action 'take-msl-2015
			:add-list '(msl-2015))
		(make-op :action 'take-msl-2020
			:add-list '(msl-2020))
		(make-op :action 'take-msl-2025
			:add-list '(msl-2025))
		(make-op :action 'take-msl-2400
			:add-list '(msl-2400))
		(make-op :action 'take-msl-2420
			:preconds '(msl-1010 msl-4020)
			:add-list '(msl-2420))
		(make-op :action 'take-msl-2430
			:add-list '(msl-2430))
		(make-op :action 'take-msl-2440
			:add-list '(msl-2440))
		(make-op :action 'take-msl-2510
			:add-list '(msl-2510))
		(make-op :action 'take-msl-3010
			:preconds '(msl-1010 msl-1020 msl-2010 msl-2020)
			:add-list '(msl-3010))
		(make-op :action 'take-msl-3015
			:add-list '(msl-3015))
		(make-op :action 'take-msl-3020
			:preconds '(msl-1010 msl-1020 msl-2010 msl-2020 msl-3010)
			:add-list '(msl-3020))
		(make-op :action 'take-msl-3025
			:add-list '(msl-3025))
		(make-op :action 'take-msl-3110
			:add-list '(msl-3110))
		(make-op :action 'take-msl-3210
			:add-list '(msl-3210))
		(make-op :action 'take-msl-4010
			:preconds '(msl-1010 msl-1020 msl-2010 msl-2020 msl-3010 msl-3020)
			:add-list '(msl-4010))
		(make-op :action 'take-msl-4015
			:add-list '(msl-4015))
		(make-op :action 'take-msl-4020
			:preconds '(msl-1010 msl-1020 msl-2010 msl-2020 msl-3010 msl-3020 msl-4010)
			:add-list '(msl-4020))
		(make-op :action 'take-msl-4025
			:add-list '(msl-4025))
		(make-op :action 'take-msl-4110
			:preconds '(msl-3110)
			:add-list '(msl-4110))
		(make-op :action 'take-msl-4510
			:add-list '(msl-4510))
		(make-op :action 'take-msl-4520
			:preconds '(msl-3010 msl-3020 msl-4510)
			:add-list '(msl-4520))
		(make-op :action 'take-mint-2210
			:add-list '(mint-2210))
		(make-op :action 'take-mint-2460
			:add-list '(mint-2460))
		(make-op :action 'take-mint-2988
			:add-list '(mint-2988))
		(make-op :action 'take-musm-1010
			:add-list '(musm-1010))
		(make-op :action 'take-musm-1020
			:add-list '(musm-1020))
		(make-op :action 'take-musm-1030
			:add-list '(musm-1030))
		(make-op :action 'take-musm-2010
			:add-list '(musm-2010))
		(make-op :action 'take-musm-2011
			:add-list '(musm-2011))
		(make-op :action 'take-musm-2020
			:add-list '(musm-2020))
		(make-op :action 'take-musm-2090
			:add-list '(musm-2090))
		(make-op :action 'take-musm-2977
			:add-list '(musm-2977))
		(make-op :action 'take-musm-2988
			:add-list '(musm-2988))
		(make-op :action 'take-musm-2999
			:add-list '(musm-2999))
		(make-op :action 'take-musm-3310
			:add-list '(musm-3310))
		(make-op :action 'take-musc-1010
			:add-list '(musc-1010))
		(make-op :action 'take-musc-1050
			:add-list '(musc-1050))
		(make-op :action 'take-musc-1100
			:add-list '(musc-1100))
		(make-op :action 'take-musc-1105
			:add-list '(musc-1105))
		(make-op :action 'take-musc-1110
			:preconds '(musc-1105)
			:add-list '(musc-1110))
		(make-op :action 'take-musc-1120
			:preconds '(musc-1110)
			:add-list '(musc-1120))
		(make-op :action 'take-musc-1130
			:preconds '(musc-1105)
			:add-list '(musc-1130))
		(make-op :action 'take-musc-1140
			:preconds '(musc-1130)
			:add-list '(musc-1140))
		(make-op :action 'take-musc-1150
			:add-list '(musc-1150))
		(make-op :action 'take-musc-1160
			:add-list '(musc-1160))
		(make-op :action 'take-musc-1170
			:preconds '(musc-1105)
			:add-list '(musc-1170))
		(make-op :action 'take-musc-1180
			:preconds '(musc-1110 musc-1170)
			:add-list '(musc-1180))
		(make-op :action 'take-musc-1185
			:preconds '(musc-1180)
			:add-list '(musc-1185))
		(make-op :action 'take-musc-1190
			:add-list '(musc-1190))
		(make-op :action 'take-musc-1195
			:add-list '(musc-1195))
		(make-op :action 'take-musc-1220
			:add-list '(musc-1220))
		(make-op :action 'take-musc-1225
			:add-list '(musc-1225))
		(make-op :action 'take-musc-1270
			:add-list '(musc-1270))
		(make-op :action 'take-musc-1280
			:add-list '(musc-1280))
		(make-op :action 'take-musc-1310
			:add-list '(musc-1310))
		(make-op :action 'take-musc-1320
			:add-list '(musc-1320))
		(make-op :action 'take-musc-1360
			:add-list '(musc-1360))
		(make-op :action 'take-musc-1430
			:add-list '(musc-1430))
		(make-op :action 'take-musc-1440
			:add-list '(musc-1440))
		(make-op :action 'take-musc-1450
			:preconds '(musc-1430)
			:add-list '(musc-1450))
		(make-op :action 'take-musc-1460
			:add-list '(musc-1460))
		(make-op :action 'take-musc-1470
			:add-list '(musc-1470))
		(make-op :action 'take-musc-1480
			:add-list '(musc-1480))
		(make-op :action 'take-musc-1484
			:add-list '(musc-1484))
		(make-op :action 'take-musc-1485
			:add-list '(musc-1485))
		(make-op :action 'take-musc-1490
			:add-list '(musc-1490))
		(make-op :action 'take-musc-1495
			:add-list '(musc-1495))
		(make-op :action 'take-musc-1500
			:add-list '(musc-1500))
		(make-op :action 'take-musc-1510
			:add-list '(musc-1510))
		(make-op :action 'take-musc-1515
			:add-list '(musc-1515))
		(make-op :action 'take-musc-1520
			:add-list '(musc-1520))
		(make-op :action 'take-musc-1525
			:add-list '(musc-1525))
		(make-op :action 'take-musc-1530
			:add-list '(musc-1530))
		(make-op :action 'take-musc-1535
			:add-list '(musc-1535))
		(make-op :action 'take-musc-1540
			:add-list '(musc-1540))
		(make-op :action 'take-musc-1545
			:add-list '(musc-1545))
		(make-op :action 'take-musc-1550
			:add-list '(musc-1550))
		(make-op :action 'take-musc-1555
			:add-list '(musc-1555))
		(make-op :action 'take-musc-1560
			:add-list '(musc-1560))
		(make-op :action 'take-musc-1580
			:add-list '(musc-1580))
		(make-op :action 'take-musc-1585
			:add-list '(musc-1585))
		(make-op :action 'take-musc-1600
			:add-list '(musc-1600))
		(make-op :action 'take-musc-1610
			:add-list '(musc-1610))
		(make-op :action 'take-musc-1620
			:add-list '(musc-1620))
		(make-op :action 'take-musc-1630
			:add-list '(musc-1630))
		(make-op :action 'take-musc-1635
			:add-list '(musc-1635))
		(make-op :action 'take-musc-1700
			:add-list '(musc-1700))
		(make-op :action 'take-musc-1705
			:add-list '(musc-1705))
		(make-op :action 'take-musc-1710
			:add-list '(musc-1710))
		(make-op :action 'take-musc-1715
			:add-list '(musc-1715))
		(make-op :action 'take-musc-1720
			:add-list '(musc-1720))
		(make-op :action 'take-musc-1725
			:add-list '(musc-1725))
		(make-op :action 'take-musc-1730
			:add-list '(musc-1730))
		(make-op :action 'take-musc-1735
			:add-list '(musc-1735))
		(make-op :action 'take-musc-1740
			:add-list '(musc-1740))
		(make-op :action 'take-musc-1745
			:add-list '(musc-1745))
		(make-op :action 'take-musc-1800
			:add-list '(musc-1800))
		(make-op :action 'take-musc-1810
			:add-list '(musc-1810))
		(make-op :action 'take-musc-1815
			:add-list '(musc-1815))
		(make-op :action 'take-musc-1820
			:add-list '(musc-1820))
		(make-op :action 'take-musc-1825
			:add-list '(musc-1825))
		(make-op :action 'take-musc-1830
			:add-list '(musc-1830))
		(make-op :action 'take-musc-1835
			:add-list '(musc-1835))
		(make-op :action 'take-musc-1840
			:add-list '(musc-1840))
		(make-op :action 'take-musc-1845
			:add-list '(musc-1845))
		(make-op :action 'take-musc-1850
			:add-list '(musc-1850))
		(make-op :action 'take-musc-1855
			:add-list '(musc-1855))
		(make-op :action 'take-musc-2110
			:preconds '(musc-1120)
			:add-list '(musc-2110))
		(make-op :action 'take-musc-2120
			:preconds '(musc-2110 musc-3120)
			:add-list '(musc-2120))
		(make-op :action 'take-musc-2130
			:preconds '(musc-1140)
			:add-list '(musc-2130))
		(make-op :action 'take-musc-2180
			:add-list '(musc-2180))
		(make-op :action 'take-musc-2210
			:preconds '(musc-4240)
			:add-list '(musc-2210))
		(make-op :action 'take-musc-2220
			:add-list '(musc-2220))
		(make-op :action 'take-musc-2225
			:preconds '(musc-1225)
			:add-list '(musc-2225))
		(make-op :action 'take-musc-2240
			:add-list '(musc-2240))
		(make-op :action 'take-musc-2265
			:preconds '(musc-1360 musc-4930)
			:add-list '(musc-2265))
		(make-op :action 'take-musc-2270
			:preconds '(musc-1280 musc-2494)
			:add-list '(musc-2270))
		(make-op :action 'take-musc-2275
			:preconds '(musc-4930 musc-1360)
			:add-list '(musc-2275))
		(make-op :action 'take-musc-2310
			:add-list '(musc-2310))
		(make-op :action 'take-musc-2320
			:preconds '(musc-2310)
			:add-list '(musc-2320))
		(make-op :action 'take-musc-2350
			:add-list '(musc-2350))
		(make-op :action 'take-musc-2360
			:preconds '(musc-1360 musc-4930)
			:add-list '(musc-2360))
		(make-op :action 'take-musc-2370
			:preconds '(musc-1360 musc-4930)
			:add-list '(musc-2370))
		(make-op :action 'take-musc-2390
			:preconds '(musc-1360 musc-4930)
			:add-list '(musc-2390))
		(make-op :action 'take-musc-2410
			:add-list '(musc-2410))
		(make-op :action 'take-musc-2415
			:preconds '(musc-1495)
			:add-list '(musc-2415))
		(make-op :action 'take-musc-2420
			:add-list '(musc-2420))
		(make-op :action 'take-musc-2430
			:add-list '(musc-2430))
		(make-op :action 'take-musc-2440
			:add-list '(musc-2440))
		(make-op :action 'take-musc-2460
			:add-list '(musc-2460))
		(make-op :action 'take-musc-2470
			:add-list '(musc-2470))
		(make-op :action 'take-musc-2490
			:add-list '(musc-2490))
		(make-op :action 'take-musc-2494
			:preconds '(musc-1484)
			:add-list '(musc-2494))
		(make-op :action 'take-musc-2495
			:preconds '(musc-1485)
			:add-list '(musc-2495))
		(make-op :action 'take-musc-2500
			:add-list '(musc-2500))
		(make-op :action 'take-musc-2505
			:preconds '(musc-1545)
			:add-list '(musc-2505))
		(make-op :action 'take-musc-2520
			:add-list '(musc-2520))
		(make-op :action 'take-musc-2525
			:preconds '(musc-1515)
			:add-list '(musc-2525))
		(make-op :action 'take-musc-2530
			:add-list '(musc-2530))
		(make-op :action 'take-musc-2535
			:preconds '(musc-1525)
			:add-list '(musc-2535))
		(make-op :action 'take-musc-2540
			:add-list '(musc-2540))
		(make-op :action 'take-musc-2545
			:preconds '(musc-1535)
			:add-list '(musc-2545))
		(make-op :action 'take-musc-2550
			:add-list '(musc-2550))
		(make-op :action 'take-musc-2560
			:add-list '(musc-2560))
		(make-op :action 'take-musc-2570
			:add-list '(musc-2570))
		(make-op :action 'take-musc-2580
			:preconds '(musc-2570)
			:add-list '(musc-2580))
		(make-op :action 'take-musc-2590
			:add-list '(musc-2590))
		(make-op :action 'take-musc-2595
			:preconds '(musc-1585)
			:add-list '(musc-2595))
		(make-op :action 'take-musc-2600
			:add-list '(musc-2600))
		(make-op :action 'take-musc-2610
			:add-list '(musc-2610))
		(make-op :action 'take-musc-2640
			:add-list '(musc-2640))
		(make-op :action 'take-musc-2645
			:preconds '(musc-1635)
			:add-list '(musc-2645))
		(make-op :action 'take-musc-2660
			:add-list '(musc-2660))
		(make-op :action 'take-musc-2670
			:add-list '(musc-2670))
		(make-op :action 'take-musc-2680
			:add-list '(musc-2680))
		(make-op :action 'take-musc-2690
			:add-list '(musc-2690))
		(make-op :action 'take-musc-2695
			:add-list '(musc-2695))
		(make-op :action 'take-musc-2700
			:add-list '(musc-2700))
		(make-op :action 'take-musc-2710
			:preconds '(musc-2700)
			:add-list '(musc-2710))
		(make-op :action 'take-musc-2730
			:preconds '(musc-3785)
			:add-list '(musc-2730))
		(make-op :action 'take-musc-2745
			:add-list '(musc-2745))
		(make-op :action 'take-musc-2750
			:add-list '(musc-2750))
		(make-op :action 'take-musc-2755
			:preconds '(musc-1705)
			:add-list '(musc-2755))
		(make-op :action 'take-musc-2760
			:add-list '(musc-2760))
		(make-op :action 'take-musc-2765
			:preconds '(musc-1715)
			:add-list '(musc-2765))
		(make-op :action 'take-musc-2770
			:add-list '(musc-2770))
		(make-op :action 'take-musc-2775
			:preconds '(musc-1725)
			:add-list '(musc-2775))
		(make-op :action 'take-musc-2780
			:add-list '(musc-2780))
		(make-op :action 'take-musc-2785
			:preconds '(musc-1735)
			:add-list '(musc-2785))
		(make-op :action 'take-musc-2790
			:add-list '(musc-2790))
		(make-op :action 'take-musc-2795
			:preconds '(musc-1745)
			:add-list '(musc-2795))
		(make-op :action 'take-musc-2800
			:add-list '(musc-2800))
		(make-op :action 'take-musc-2810
			:add-list '(musc-2810))
		(make-op :action 'take-musc-2850
			:add-list '(musc-2850))
		(make-op :action 'take-musc-2855
			:preconds '(musc-1815)
			:add-list '(musc-2855))
		(make-op :action 'take-musc-2860
			:add-list '(musc-2860))
		(make-op :action 'take-musc-2865
			:preconds '(musc-1825)
			:add-list '(musc-2865))
		(make-op :action 'take-musc-2870
			:add-list '(musc-2870))
		(make-op :action 'take-musc-2875
			:preconds '(musc-1835)
			:add-list '(musc-2875))
		(make-op :action 'take-musc-2880
			:add-list '(musc-2880))
		(make-op :action 'take-musc-2885
			:preconds '(musc-1845)
			:add-list '(musc-2885))
		(make-op :action 'take-musc-2890
			:add-list '(musc-2890))
		(make-op :action 'take-musc-2895
			:preconds '(musc-1855)
			:add-list '(musc-2895))
		(make-op :action 'take-musc-2920
			:add-list '(musc-2920))
		(make-op :action 'take-musc-2977
			:add-list '(musc-2977))
		(make-op :action 'take-musc-2988
			:add-list '(musc-2988))
		(make-op :action 'take-musc-3010
			:add-list '(musc-3010))
		(make-op :action 'take-musc-3020
			:add-list '(musc-3020))
		(make-op :action 'take-musc-3030
			:add-list '(musc-3030))
		(make-op :action 'take-musc-3040
			:add-list '(musc-3040))
		(make-op :action 'take-musc-3100
			:add-list '(musc-3100))
		(make-op :action 'take-musc-3110
			:add-list '(musc-3110))
		(make-op :action 'take-musc-3120
			:preconds '(musc-3110)
			:add-list '(musc-3120))
		(make-op :action 'take-musc-3170
			:preconds '(musc-2110)
			:add-list '(musc-3170))
		(make-op :action 'take-musc-3180
			:preconds '(musc-1110 musc-1120 musc-2110 musc-2180)
			:add-list '(musc-3180))
		(make-op :action 'take-musc-3190
			:preconds '(musc-3120)
			:add-list '(musc-3190))
		(make-op :action 'take-musc-3210
			:preconds '(musc-2225)
			:add-list '(musc-3210))
		(make-op :action 'take-musc-3220
			:preconds '(sced-3300 sced-4300)
			:add-list '(musc-3220))
		(make-op :action 'take-musc-3230
			:add-list '(musc-3230))
		(make-op :action 'take-musc-3240
			:preconds '(sced-3300 sced-4300)
			:add-list '(musc-3240))
		(make-op :action 'take-musc-3260
			:preconds '(musc-1010)
			:add-list '(musc-3260))
		(make-op :action 'take-musc-3310
			:preconds '(musc-2320)
			:add-list '(musc-3310))
		(make-op :action 'take-musc-3320
			:add-list '(musc-3320))
		(make-op :action 'take-musc-3331
			:preconds '(musc-2320)
			:add-list '(musc-3331))
		(make-op :action 'take-musc-3332
			:preconds '(musc-3331)
			:add-list '(musc-3332))
		(make-op :action 'take-musc-3333
			:preconds '(musc-3332)
			:add-list '(musc-3333))
		(make-op :action 'take-musc-3334
			:preconds '(musc-3333)
			:add-list '(musc-3334))
		(make-op :action 'take-musc-3360
			:add-list '(musc-3360))
		(make-op :action 'take-musc-3370
			:preconds '(musc-3360)
			:add-list '(musc-3370))
		(make-op :action 'take-musc-3410
			:add-list '(musc-3410))
		(make-op :action 'take-musc-3440
			:add-list '(musc-3440))
		(make-op :action 'take-musc-3445
			:preconds '(musc-2494)
			:add-list '(musc-3445))
		(make-op :action 'take-musc-3450
			:preconds '(musc-2495)
			:add-list '(musc-3450))
		(make-op :action 'take-musc-3460
			:add-list '(musc-3460))
		(make-op :action 'take-musc-3470
			:add-list '(musc-3470))
		(make-op :action 'take-musc-3480
			:preconds '(musc-2415)
			:add-list '(musc-3480))
		(make-op :action 'take-musc-3500
			:add-list '(musc-3500))
		(make-op :action 'take-musc-3510
			:add-list '(musc-3510))
		(make-op :action 'take-musc-3515
			:add-list '(musc-3515))
		(make-op :action 'take-musc-3520
			:add-list '(musc-3520))
		(make-op :action 'take-musc-3530
			:add-list '(musc-3530))
		(make-op :action 'take-musc-3540
			:add-list '(musc-3540))
		(make-op :action 'take-musc-3550
			:preconds '(musc-2595)
			:add-list '(musc-3550))
		(make-op :action 'take-musc-3560
			:add-list '(musc-3560))
		(make-op :action 'take-musc-3570
			:add-list '(musc-3570))
		(make-op :action 'take-musc-3580
			:add-list '(musc-3580))
		(make-op :action 'take-musc-3590
			:add-list '(musc-3590))
		(make-op :action 'take-musc-3600
			:add-list '(musc-3600))
		(make-op :action 'take-musc-3610
			:add-list '(musc-3610))
		(make-op :action 'take-musc-3620
			:add-list '(musc-3620))
		(make-op :action 'take-musc-3630
			:add-list '(musc-3630))
		(make-op :action 'take-musc-3640
			:add-list '(musc-3640))
		(make-op :action 'take-musc-3660
			:add-list '(musc-3660))
		(make-op :action 'take-musc-3670
			:preconds '(musc-2645)
			:add-list '(musc-3670))
		(make-op :action 'take-musc-3700
			:add-list '(musc-3700))
		(make-op :action 'take-musc-3710
			:preconds '(musc-2755)
			:add-list '(musc-3710))
		(make-op :action 'take-musc-3720
			:preconds '(musc-2765)
			:add-list '(musc-3720))
		(make-op :action 'take-musc-3730
			:preconds '(musc-2775 musc-2775)
			:add-list '(musc-3730))
		(make-op :action 'take-musc-3740
			:preconds '(musc-2785)
			:add-list '(musc-3740))
		(make-op :action 'take-musc-3750
			:preconds '(musc-2795)
			:add-list '(musc-3750))
		(make-op :action 'take-musc-3760
			:add-list '(musc-3760))
		(make-op :action 'take-musc-3770
			:add-list '(musc-3770))
		(make-op :action 'take-musc-3780
			:add-list '(musc-3780))
		(make-op :action 'take-musc-3785
			:add-list '(musc-3785))
		(make-op :action 'take-musc-3790
			:add-list '(musc-3790))
		(make-op :action 'take-musc-3795
			:add-list '(musc-3795))
		(make-op :action 'take-musc-3800
			:add-list '(musc-3800))
		(make-op :action 'take-musc-3810
			:preconds '(musc-2855)
			:add-list '(musc-3810))
		(make-op :action 'take-musc-3820
			:preconds '(musc-2865)
			:add-list '(musc-3820))
		(make-op :action 'take-musc-3830
			:preconds '(musc-2875)
			:add-list '(musc-3830))
		(make-op :action 'take-musc-3840
			:preconds '(musc-2885)
			:add-list '(musc-3840))
		(make-op :action 'take-musc-3850
			:add-list '(musc-3850))
		(make-op :action 'take-musc-3860
			:preconds '(musc-2895)
			:add-list '(musc-3860))
		(make-op :action 'take-musc-3870
			:add-list '(musc-3870))
		(make-op :action 'take-musc-3900
			:preconds '(musc-2110 musc-2130)
			:add-list '(musc-3900))
		(make-op :action 'take-musc-3905
			:preconds '(musc-3900)
			:add-list '(musc-3905))
		(make-op :action 'take-musc-3910
			:add-list '(musc-3910))
		(make-op :action 'take-musc-3915
			:add-list '(musc-3915))
		(make-op :action 'take-musc-3920
			:add-list '(musc-3920))
		(make-op :action 'take-musc-3930
			:add-list '(musc-3930))
		(make-op :action 'take-musc-3940
			:preconds '(musc-2180)
			:add-list '(musc-3940))
		(make-op :action 'take-musc-3950
			:add-list '(musc-3950))
		(make-op :action 'take-musc-3970
			:preconds '(musc-2240)
			:add-list '(musc-3970))
		(make-op :action 'take-musc-4240
			:add-list '(musc-4240))
		(make-op :action 'take-musc-4310
			:preconds '(musc-3310)
			:add-list '(musc-4310))
		(make-op :action 'take-musc-4320
			:preconds '(stat-1040)
			:add-list '(musc-4320))
		(make-op :action 'take-musc-4330
			:preconds '(musc-4310)
			:add-list '(musc-4330))
		(make-op :action 'take-musc-4340
			:preconds '(musc-4330)
			:add-list '(musc-4340))
		(make-op :action 'take-musc-4410
			:preconds '(musc-1430 musc-1440)
			:add-list '(musc-4410))
		(make-op :action 'take-musc-4460
			:add-list '(musc-4460))
		(make-op :action 'take-musc-4470
			:preconds '(musc-6470)
			:add-list '(musc-4470))
		(make-op :action 'take-musc-4500
			:add-list '(musc-4500))
		(make-op :action 'take-musc-4510
			:preconds '(musc-2545)
			:add-list '(musc-4510))
		(make-op :action 'take-musc-4520
			:preconds '(musc-2535)
			:add-list '(musc-4520))
		(make-op :action 'take-musc-4530
			:preconds '(musc-2525)
			:add-list '(musc-4530))
		(make-op :action 'take-musc-4540
			:preconds '(musc-2505)
			:add-list '(musc-4540))
		(make-op :action 'take-musc-4550
			:add-list '(musc-4550))
		(make-op :action 'take-musc-4600
			:add-list '(musc-4600))
		(make-op :action 'take-musc-4620
			:add-list '(musc-4620))
		(make-op :action 'take-musc-4650
			:add-list '(musc-4650))
		(make-op :action 'take-musc-4700
			:add-list '(musc-4700))
		(make-op :action 'take-musc-4710
			:add-list '(musc-4710))
		(make-op :action 'take-musc-4720
			:add-list '(musc-4720))
		(make-op :action 'take-musc-4730
			:preconds '(engl-2010)
			:add-list '(musc-4730))
		(make-op :action 'take-musc-4915
			:preconds '(musc-6915)
			:add-list '(musc-4915))
		(make-op :action 'take-musc-4920
			:add-list '(musc-4920))
		(make-op :action 'take-musc-4930
			:add-list '(musc-4930))
		(make-op :action 'take-musc-4935
			:add-list '(musc-4935))
		(make-op :action 'take-musc-4940
			:add-list '(musc-4940))
		(make-op :action 'take-musc-5261
			:add-list '(musc-5261))
		(make-op :action 'take-musc-5262
			:preconds '(musc-5261)
			:add-list '(musc-5262))
		(make-op :action 'take-musc-5263
			:preconds '(musc-5261 musc-5262)
			:add-list '(musc-5263))
		(make-op :action 'take-musc-5420
			:add-list '(musc-5420))
		(make-op :action 'take-musc-5500
			:preconds '(musc-5630)
			:add-list '(musc-5500))
		(make-op :action 'take-musc-5630
			:preconds '(musc-5500)
			:add-list '(musc-5630))
		(make-op :action 'take-musc-5980
			:add-list '(musc-5980))
		(make-op :action 'take-tent-2200
			:add-list '(tent-2200))
		(make-op :action 'take-tent-2205
			:add-list '(tent-2205))
		(make-op :action 'take-tent-2210
			:add-list '(tent-2210))
		(make-op :action 'take-tent-2215
			:add-list '(tent-2215))
		(make-op :action 'take-tent-2220
			:add-list '(tent-2220))
		(make-op :action 'take-nas-1010
			:add-list '(nas-1010))
		(make-op :action 'take-nr-4250
			:add-list '(nr-4250))
		(make-op :action 'take-nav-1010
			:add-list '(nav-1010))
		(make-op :action 'take-nav-1030
			:add-list '(nav-1030))
		(make-op :action 'take-nav-2010
			:add-list '(nav-2010))
		(make-op :action 'take-nav-2030
			:add-list '(nav-2030))
		(make-op :action 'take-nav-2070
			:add-list '(nav-2070))
		(make-op :action 'take-nav-2080
			:add-list '(nav-2080))
		(make-op :action 'take-nav-2988
			:add-list '(nav-2988))
		(make-op :action 'take-nav-3040
			:add-list '(nav-3040))
		(make-op :action 'take-nav-3050
			:add-list '(nav-3050))
		(make-op :action 'take-nav-4400
			:add-list '(nav-4400))
		(make-op :action 'take-nav-4410
			:add-list '(nav-4410))
		(make-op :action 'take-tena-1000
			:add-list '(tena-1000))
		(make-op :action 'take-nurs-1010
			:add-list '(nurs-1010))
		(make-op :action 'take-nurs-1020
			:add-list '(nurs-1020))
		(make-op :action 'take-nurs-1030
			:add-list '(nurs-1030))
		(make-op :action 'take-nurs-1042
			:add-list '(nurs-1042))
		(make-op :action 'take-nurs-1110
			:add-list '(nurs-1110))
		(make-op :action 'take-nurs-1122
			:add-list '(nurs-1122))
		(make-op :action 'take-nurs-1220
			:add-list '(nurs-1220))
		(make-op :action 'take-nurs-1230
			:add-list '(nurs-1230))
		(make-op :action 'take-nurs-1240
			:add-list '(nurs-1240))
		(make-op :action 'take-nurs-2020
			:add-list '(nurs-2020))
		(make-op :action 'take-nurs-2030
			:add-list '(nurs-2030))
		(make-op :action 'take-nurs-2140
			:add-list '(nurs-2140))
		(make-op :action 'take-nurs-2220
			:add-list '(nurs-2220))
		(make-op :action 'take-nurs-2230
			:add-list '(nurs-2230))
		(make-op :action 'take-nurs-2240
			:add-list '(nurs-2240))
		(make-op :action 'take-nurs-2250
			:add-list '(nurs-2250))
		(make-op :action 'take-nurs-2500
			:preconds '(biol-2320 biol-2420)
			:add-list '(nurs-2500))
		(make-op :action 'take-nurs-2988
			:add-list '(nurs-2988))
		(make-op :action 'take-nurs-3000
			:add-list '(nurs-3000))
		(make-op :action 'take-nurs-3010
			:preconds '(nurs-3015)
			:add-list '(nurs-3010))
		(make-op :action 'take-nurs-3015
			:preconds '(nurs-3010)
			:add-list '(nurs-3015))
		(make-op :action 'take-nurs-3020
			:preconds '(nurs-3025)
			:add-list '(nurs-3020))
		(make-op :action 'take-nurs-3025
			:preconds '(nurs-3020)
			:add-list '(nurs-3025))
		(make-op :action 'take-nurs-3030
			:preconds '(nurs-3035)
			:add-list '(nurs-3030))
		(make-op :action 'take-nurs-3035
			:preconds '(nurs-3030)
			:add-list '(nurs-3035))
		(make-op :action 'take-nurs-3040
			:add-list '(nurs-3040))
		(make-op :action 'take-nurs-3210
			:preconds '(nurs-3215)
			:add-list '(nurs-3210))
		(make-op :action 'take-nurs-3215
			:preconds '(nurs-3210)
			:add-list '(nurs-3215))
		(make-op :action 'take-nurs-3220
			:preconds '(nurs-3225)
			:add-list '(nurs-3220))
		(make-op :action 'take-nurs-3225
			:preconds '(nurs-3220)
			:add-list '(nurs-3225))
		(make-op :action 'take-nurs-3230
			:add-list '(nurs-3230))
		(make-op :action 'take-nurs-3240
			:add-list '(nurs-3240))
		(make-op :action 'take-nurs-3250
			:add-list '(nurs-3250))
		(make-op :action 'take-nurs-3300
			:preconds '(nurs-3000)
			:add-list '(nurs-3300))
		(make-op :action 'take-nurs-4010
			:preconds '(nurs-3230)
			:add-list '(nurs-4010))
		(make-op :action 'take-nurs-4015
			:preconds '(nurs-3230 nurs-4010)
			:add-list '(nurs-4015))
		(make-op :action 'take-nurs-4020
			:preconds '(nurs-4025)
			:add-list '(nurs-4020))
		(make-op :action 'take-nurs-4025
			:preconds '(nurs-4020)
			:add-list '(nurs-4025))
		(make-op :action 'take-nurs-4100
			:preconds '(nurs-3000)
			:add-list '(nurs-4100))
		(make-op :action 'take-nurs-4210
			:preconds '(nurs-4215)
			:add-list '(nurs-4210))
		(make-op :action 'take-nurs-4215
			:add-list '(nurs-4215))
		(make-op :action 'take-nurs-4250
			:add-list '(nurs-4250))
		(make-op :action 'take-nurs-4300
			:preconds '(nurs-3000 nurs-3230)
			:add-list '(nurs-4300))
		(make-op :action 'take-ndfs-1000
			:add-list '(ndfs-1000))
		(make-op :action 'take-ndfs-1010
			:add-list '(ndfs-1010))
		(make-op :action 'take-ndfs-1020
			:add-list '(ndfs-1020))
		(make-op :action 'take-ndfs-1030
			:add-list '(ndfs-1030))
		(make-op :action 'take-ndfs-1050
			:add-list '(ndfs-1050))
		(make-op :action 'take-ndfs-1240
			:add-list '(ndfs-1240))
		(make-op :action 'take-ndfs-1250
			:add-list '(ndfs-1250))
		(make-op :action 'take-ndfs-1600
			:add-list '(ndfs-1600))
		(make-op :action 'take-ndfs-2020
			:preconds '(ndfs-1020)
			:add-list '(ndfs-2020))
		(make-op :action 'take-ndfs-2030
			:preconds '(ndfs-1020)
			:add-list '(ndfs-2030))
		(make-op :action 'take-ndfs-2040
			:add-list '(ndfs-2040))
		(make-op :action 'take-ndfs-2500
			:add-list '(ndfs-2500))
		(make-op :action 'take-ndfs-3010
			:add-list '(ndfs-3010))
		(make-op :action 'take-ndfs-3040
			:preconds '(ndfs-1020)
			:add-list '(ndfs-3040))
		(make-op :action 'take-ndfs-3045
			:preconds '(ndfs-3040)
			:add-list '(ndfs-3045))
		(make-op :action 'take-ndfs-3050
			:add-list '(ndfs-3050))
		(make-op :action 'take-ndfs-3060
			:preconds '(ndfs-3050 ndfs-3050)
			:add-list '(ndfs-3060))
		(make-op :action 'take-ndfs-3070
			:preconds '(chem-1120 chem-2300 chem-2310)
			:add-list '(ndfs-3070))
		(make-op :action 'take-ndfs-3080
			:preconds '(ndfs-1020)
			:add-list '(ndfs-3080))
		(make-op :action 'take-ndfs-3090
			:add-list '(ndfs-3090))
		(make-op :action 'take-ndfs-3110
			:add-list '(ndfs-3110))
		(make-op :action 'take-ndfs-4020
			:preconds '(ndfs-1020 chem-3700 biol-2420)
			:add-list '(ndfs-4020))
		(make-op :action 'take-ndfs-4250
			:add-list '(ndfs-4250))
		(make-op :action 'take-ndfs-4420
			:preconds '(stat-1040 math-1050)
			:add-list '(ndfs-4420))
		(make-op :action 'take-ndfs-4440
			:preconds '(ndfs-2040 phys-2110)
			:add-list '(ndfs-4440))
		(make-op :action 'take-ndfs-4450
			:preconds '(ndfs-3040)
			:add-list '(ndfs-4450))
		(make-op :action 'take-ndfs-4460
			:preconds '(ndfs-4560)
			:add-list '(ndfs-4460))
		(make-op :action 'take-ndfs-4500
			:add-list '(ndfs-4500))
		(make-op :action 'take-ndfs-4560
			:preconds '(ndfs-3045)
			:add-list '(ndfs-4560))
		(make-op :action 'take-ndfs-4660
			:preconds '(ndfs-3040 ndfs-4560)
			:add-list '(ndfs-4660))
		(make-op :action 'take-ndfs-4710
			:preconds '(ndfs-3070)
			:add-list '(ndfs-4710))
		(make-op :action 'take-ndfs-4720
			:preconds '(ndfs-4710)
			:add-list '(ndfs-4720))
		(make-op :action 'take-ndfs-4750
			:add-list '(ndfs-4750))
		(make-op :action 'take-ndfs-4760
			:add-list '(ndfs-4760))
		(make-op :action 'take-ndfs-4780
			:preconds '(ndfs-3040)
			:add-list '(ndfs-4780))
		(make-op :action 'take-ndfs-4790
			:preconds '(ndfs-4780)
			:add-list '(ndfs-4790))
		(make-op :action 'take-ndfs-4900
			:add-list '(ndfs-4900))
		(make-op :action 'take-ndfs-5010
			:preconds '(ndfs-6010)
			:add-list '(ndfs-5010))
		(make-op :action 'take-ndfs-5020
			:preconds '(ndfs-6020)
			:add-list '(ndfs-5020))
		(make-op :action 'take-ndfs-5030
			:preconds '(ndfs-5110 ndfs-6110 ndfs-5560 ndfs-6560)
			:add-list '(ndfs-5030))
		(make-op :action 'take-ndfs-5040
			:preconds '(ndfs-5030 ndfs-6040)
			:add-list '(ndfs-5040))
		(make-op :action 'take-ndfs-5050
			:add-list '(ndfs-5050))
		(make-op :action 'take-ndfs-5060
			:add-list '(ndfs-5060))
		(make-op :action 'take-ndfs-5080
			:add-list '(ndfs-5080))
		(make-op :action 'take-ndfs-5090
			:add-list '(ndfs-5090))
		(make-op :action 'take-ndfs-5100
			:preconds '(stat-3000 stat-2000 ndfs-6100)
			:add-list '(ndfs-5100))
		(make-op :action 'take-ndfs-5110
			:preconds '(biol-2060 biol-3300)
			:add-list '(ndfs-5110))
		(make-op :action 'take-ndfs-5111
			:preconds '(ndfs-5110)
			:add-list '(ndfs-5111))
		(make-op :action 'take-ndfs-5150
			:preconds '(ndfs-3040 ndfs-4560)
			:add-list '(ndfs-5150))
		(make-op :action 'take-ndfs-5160
			:preconds '(biol-5160 psc-5160)
			:add-list '(ndfs-5160))
		(make-op :action 'take-ndfs-5200
			:preconds '(stat-1040 stat-1045 ndfs-1020 ndfs-6200)
			:add-list '(ndfs-5200))
		(make-op :action 'take-ndfs-5210
			:preconds '(ndfs-4020 stat-1040 stat-1045 ndfs-6210)
			:add-list '(ndfs-5210))
		(make-op :action 'take-ndfs-5220
			:preconds '(chem-3700 advs-5220 biol-5220 ndfs-6220)
			:add-list '(ndfs-5220))
		(make-op :action 'take-ndfs-5230
			:preconds '(ndfs-1020 ndfs-6230)
			:add-list '(ndfs-5230))
		(make-op :action 'take-ndfs-5250
			:preconds '(ndfs-1020)
			:add-list '(ndfs-5250))
		(make-op :action 'take-ndfs-5260
			:preconds '(chem-3700 chem-5710 biol-3060 advs-5260 biol-5260 psc-5260)
			:add-list '(ndfs-5260))
		(make-op :action 'take-ndfs-5300
			:preconds '(ndfs-4020 ndfs-6300)
			:add-list '(ndfs-5300))
		(make-op :action 'take-ndfs-5310
			:preconds '(ndfs-4020 ndfs-6310)
			:add-list '(ndfs-5310))
		(make-op :action 'take-ndfs-5320
			:preconds '(ndfs-2030 ndfs-4020 ndfs-6320)
			:add-list '(ndfs-5320))
		(make-op :action 'take-ndfs-5370
			:add-list '(ndfs-5370))
		(make-op :action 'take-ndfs-5400
			:preconds '(ndfs-1020 biol-2420 chem-3700 ndfs-6400)
			:add-list '(ndfs-5400))
		(make-op :action 'take-ndfs-5410
			:preconds '(chem-3700 ndfs-6410)
			:add-list '(ndfs-5410))
		(make-op :action 'take-ndfs-5420
			:preconds '(ndfs-6420)
			:add-list '(ndfs-5420))
		(make-op :action 'take-ndfs-5450
			:preconds '(aste-5450)
			:add-list '(ndfs-5450))
		(make-op :action 'take-ndfs-5500
			:preconds '(ndfs-5560 ndfs-6560 ndfs-6500)
			:add-list '(ndfs-5500))
		(make-op :action 'take-ndfs-5510
			:add-list '(ndfs-5510))
		(make-op :action 'take-ndfs-5520
			:preconds '(ndfs-1020 ndfs-6520)
			:add-list '(ndfs-5520))
		(make-op :action 'take-ndfs-5560
			:preconds '(chem-3700 chem-3710 ndfs-3070 ndfs-6560)
			:add-list '(ndfs-5560))
		(make-op :action 'take-ndfs-5610
			:preconds '(beng-3200 beng-5610 ndfs-6610)
			:add-list '(ndfs-5610))
		(make-op :action 'take-ndfs-5750
			:add-list '(ndfs-5750))
		(make-op :action 'take-ndfs-5800
			:preconds '(ndfs-2020 ndfs-2030 ndfs-3080 ndfs-6800)
			:add-list '(ndfs-5800))
		(make-op :action 'take-ndfs-5830
			:preconds '(ndfs-6830)
			:add-list '(ndfs-5830))
		(make-op :action 'take-ndfs-5920
			:preconds '(ndfs-5100 ndfs-6100 ndfs-5110 ndfs-5560)
			:add-list '(ndfs-5920))
		(make-op :action 'take-oss-1050
			:add-list '(oss-1050))
		(make-op :action 'take-oss-1060
			:add-list '(oss-1060))
		(make-op :action 'take-oss-1110
			:add-list '(oss-1110))
		(make-op :action 'take-oss-1400
			:add-list '(oss-1400))
		(make-op :action 'take-oss-1410
			:add-list '(oss-1410))
		(make-op :action 'take-oss-1420
			:add-list '(oss-1420))
		(make-op :action 'take-oss-1450
			:preconds '(oss-1050)
			:add-list '(oss-1450))
		(make-op :action 'take-oss-1550
			:add-list '(oss-1550))
		(make-op :action 'take-oss-2300
			:preconds '(oss-1400)
			:add-list '(oss-2300))
		(make-op :action 'take-oss-2400
			:preconds '(oss-1400)
			:add-list '(oss-2400))
		(make-op :action 'take-oss-2450
			:preconds '(oss-1400)
			:add-list '(oss-2450))
		(make-op :action 'take-oss-2500
			:preconds '(oss-2450)
			:add-list '(oss-2500))
		(make-op :action 'take-oss-2520
			:preconds '(oss-1400)
			:add-list '(oss-2520))
		(make-op :action 'take-oss-2600
			:preconds '(oss-1550 oss-2520 bus-3200)
			:add-list '(oss-2600))
		(make-op :action 'take-oss-2800
			:add-list '(oss-2800))
		(make-op :action 'take-opdd-1000
			:add-list '(opdd-1000))
		(make-op :action 'take-opdd-1050
			:add-list '(opdd-1050))
		(make-op :action 'take-opdd-1100
			:add-list '(opdd-1100))
		(make-op :action 'take-opdd-1700
			:add-list '(opdd-1700))
		(make-op :action 'take-opdd-1750
			:add-list '(opdd-1750))
		(make-op :action 'take-opdd-2040
			:preconds '(fcse-1040)
			:add-list '(opdd-2040))
		(make-op :action 'take-opdd-2420
			:preconds '(tee-1010)
			:add-list '(opdd-2420))
		(make-op :action 'take-opdd-2430
			:preconds '(tee-1200 tesy-1200 mae-1200 eddt-2620)
			:add-list '(opdd-2430))
		(make-op :action 'take-opdd-3030
			:add-list '(opdd-3030))
		(make-op :action 'take-opdd-3400
			:add-list '(opdd-3400))
		(make-op :action 'take-opdd-3500
			:add-list '(opdd-3500))
		(make-op :action 'take-opdd-3600
			:add-list '(opdd-3600))
		(make-op :action 'take-opdd-3700
			:add-list '(opdd-3700))
		(make-op :action 'take-opdd-3760
			:add-list '(opdd-3760))
		(make-op :action 'take-opdd-3770
			:preconds '(opdd-3760)
			:add-list '(opdd-3770))
		(make-op :action 'take-opdd-3900
			:add-list '(opdd-3900))
		(make-op :action 'take-opdd-4040
			:add-list '(opdd-4040))
		(make-op :action 'take-opdd-4240
			:add-list '(opdd-4240))
		(make-op :action 'take-opdd-4250
			:add-list '(opdd-4250))
		(make-op :action 'take-opdd-4440
			:add-list '(opdd-4440))
		(make-op :action 'take-opdd-4510
			:add-list '(opdd-4510))
		(make-op :action 'take-opdd-4750
			:preconds '(opdd-3760)
			:add-list '(opdd-4750))
		(make-op :action 'take-opdd-4760
			:preconds '(opdd-4750)
			:add-list '(opdd-4760))
		(make-op :action 'take-opdd-4770
			:preconds '(opdd-4750)
			:add-list '(opdd-4770))
		(make-op :action 'take-opdd-4900
			:preconds '(opdd-3030)
			:add-list '(opdd-4900))
		(make-op :action 'take-tept-1010
			:preconds '(stat-1040 math-1050 engl-1010)
			:add-list '(tept-1010))
		(make-op :action 'take-tept-1100
			:preconds '(stat-1040 math-1050 engl-1010)
			:add-list '(tept-1100))
		(make-op :action 'take-tept-1320
			:preconds '(stat-1040 math-1050 engl-1010)
			:add-list '(tept-1320))
		(make-op :action 'take-tept-1620
			:add-list '(tept-1620))
		(make-op :action 'take-tept-1910
			:preconds '(stat-1040 math-1050 engl-1010)
			:add-list '(tept-1910))
		(make-op :action 'take-teph-1860
			:preconds '(teph-1865)
			:add-list '(teph-1860))
		(make-op :action 'take-teph-1865
			:preconds '(teph-1860)
			:add-list '(teph-1865))
		(make-op :action 'take-phil-1000
			:add-list '(phil-1000))
		(make-op :action 'take-phil-1120
			:add-list '(phil-1120))
		(make-op :action 'take-phil-1250
			:add-list '(phil-1250))
		(make-op :action 'take-phil-1320
			:add-list '(phil-1320))
		(make-op :action 'take-phil-1500
			:add-list '(phil-1500))
		(make-op :action 'take-phil-2200
			:preconds '(math-1030 math-1050 stat-1040 stat-1045)
			:add-list '(phil-2200))
		(make-op :action 'take-phil-2400
			:add-list '(phil-2400))
		(make-op :action 'take-phil-2600
			:add-list '(phil-2600))
		(make-op :action 'take-phil-2988
			:add-list '(phil-2988))
		(make-op :action 'take-phil-3010
			:add-list '(phil-3010))
		(make-op :action 'take-phil-3020
			:add-list '(phil-3020))
		(make-op :action 'take-phil-3500
			:add-list '(phil-3500))
		(make-op :action 'take-phil-3520
			:add-list '(phil-3520))
		(make-op :action 'take-phil-3530
			:add-list '(phil-3530))
		(make-op :action 'take-phil-3550
			:add-list '(phil-3550))
		(make-op :action 'take-phil-3580
			:add-list '(phil-3580))
		(make-op :action 'take-phil-3600
			:add-list '(phil-3600))
		(make-op :action 'take-phil-3700
			:add-list '(phil-3700))
		(make-op :action 'take-phil-3710
			:add-list '(phil-3710))
		(make-op :action 'take-phil-3720
			:add-list '(phil-3720))
		(make-op :action 'take-phil-3800
			:add-list '(phil-3800))
		(make-op :action 'take-phil-3810
			:add-list '(phil-3810))
		(make-op :action 'take-phil-3820
			:add-list '(phil-3820))
		(make-op :action 'take-phil-3990
			:add-list '(phil-3990))
		(make-op :action 'take-phil-4250
			:add-list '(phil-4250))
		(make-op :action 'take-phil-4300
			:add-list '(phil-4300))
		(make-op :action 'take-phil-4310
			:add-list '(phil-4310))
		(make-op :action 'take-phil-4400
			:add-list '(phil-4400))
		(make-op :action 'take-phil-4410
			:add-list '(phil-4410))
		(make-op :action 'take-phil-4500
			:add-list '(phil-4500))
		(make-op :action 'take-phil-4530
			:preconds '(phil-6530)
			:add-list '(phil-4530))
		(make-op :action 'take-phil-4600
			:add-list '(phil-4600))
		(make-op :action 'take-phil-4900
			:preconds '(phil-3010 phil-3020)
			:add-list '(phil-4900))
		(make-op :action 'take-phil-4910
			:add-list '(phil-4910))
		(make-op :action 'take-phil-4920
			:add-list '(phil-4920))
		(make-op :action 'take-phil-4930
			:add-list '(phil-4930))
		(make-op :action 'take-phil-4950
			:add-list '(phil-4950))
		(make-op :action 'take-phil-4990
			:add-list '(phil-4990))
		(make-op :action 'take-phil-5400
			:preconds '(cai-5400)
			:add-list '(phil-5400))
		(make-op :action 'take-pe-1010
			:add-list '(pe-1010))
		(make-op :action 'take-pe-1014
			:add-list '(pe-1014))
		(make-op :action 'take-pe-1016
			:add-list '(pe-1016))
		(make-op :action 'take-pe-1030
			:add-list '(pe-1030))
		(make-op :action 'take-pe-1046
			:add-list '(pe-1046))
		(make-op :action 'take-pe-1055
			:add-list '(pe-1055))
		(make-op :action 'take-pe-1057
			:add-list '(pe-1057))
		(make-op :action 'take-pe-1058
			:add-list '(pe-1058))
		(make-op :action 'take-pe-1063
			:add-list '(pe-1063))
		(make-op :action 'take-pe-1070
			:add-list '(pe-1070))
		(make-op :action 'take-pe-1080
			:add-list '(pe-1080))
		(make-op :action 'take-pe-1085
			:add-list '(pe-1085))
		(make-op :action 'take-pe-1090
			:add-list '(pe-1090))
		(make-op :action 'take-pe-1097
			:add-list '(pe-1097))
		(make-op :action 'take-pe-1100
			:add-list '(pe-1100))
		(make-op :action 'take-pe-1101
			:add-list '(pe-1101))
		(make-op :action 'take-pe-1103
			:add-list '(pe-1103))
		(make-op :action 'take-pe-1105
			:add-list '(pe-1105))
		(make-op :action 'take-pe-1110
			:add-list '(pe-1110))
		(make-op :action 'take-pe-1111
			:add-list '(pe-1111))
		(make-op :action 'take-pe-1112
			:add-list '(pe-1112))
		(make-op :action 'take-pe-1115
			:add-list '(pe-1115))
		(make-op :action 'take-pe-1116
			:add-list '(pe-1116))
		(make-op :action 'take-pe-1120
			:add-list '(pe-1120))
		(make-op :action 'take-pe-1130
			:add-list '(pe-1130))
		(make-op :action 'take-pe-1131
			:add-list '(pe-1131))
		(make-op :action 'take-pe-1135
			:add-list '(pe-1135))
		(make-op :action 'take-pe-1136
			:add-list '(pe-1136))
		(make-op :action 'take-pe-1140
			:add-list '(pe-1140))
		(make-op :action 'take-pe-1141
			:add-list '(pe-1141))
		(make-op :action 'take-pe-1145
			:add-list '(pe-1145))
		(make-op :action 'take-pe-1146
			:add-list '(pe-1146))
		(make-op :action 'take-pe-1150
			:add-list '(pe-1150))
		(make-op :action 'take-pe-1151
			:add-list '(pe-1151))
		(make-op :action 'take-pe-1152
			:add-list '(pe-1152))
		(make-op :action 'take-pe-1155
			:add-list '(pe-1155))
		(make-op :action 'take-pe-1160
			:add-list '(pe-1160))
		(make-op :action 'take-pe-1170
			:add-list '(pe-1170))
		(make-op :action 'take-pe-1171
			:add-list '(pe-1171))
		(make-op :action 'take-pe-1200
			:add-list '(pe-1200))
		(make-op :action 'take-pe-1210
			:add-list '(pe-1210))
		(make-op :action 'take-pe-1211
			:add-list '(pe-1211))
		(make-op :action 'take-pe-1212
			:add-list '(pe-1212))
		(make-op :action 'take-pe-1225
			:add-list '(pe-1225))
		(make-op :action 'take-pe-1230
			:add-list '(pe-1230))
		(make-op :action 'take-pe-1235
			:add-list '(pe-1235))
		(make-op :action 'take-pe-1245
			:add-list '(pe-1245))
		(make-op :action 'take-pe-1246
			:add-list '(pe-1246))
		(make-op :action 'take-pe-1260
			:add-list '(pe-1260))
		(make-op :action 'take-pe-1261
			:add-list '(pe-1261))
		(make-op :action 'take-pe-1265
			:add-list '(pe-1265))
		(make-op :action 'take-pe-1300
			:add-list '(pe-1300))
		(make-op :action 'take-pe-1301
			:add-list '(pe-1301))
		(make-op :action 'take-pe-1315
			:add-list '(pe-1315))
		(make-op :action 'take-pe-1340
			:add-list '(pe-1340))
		(make-op :action 'take-pe-1345
			:add-list '(pe-1345))
		(make-op :action 'take-pe-1350
			:add-list '(pe-1350))
		(make-op :action 'take-pe-1400
			:add-list '(pe-1400))
		(make-op :action 'take-pe-1407
			:add-list '(pe-1407))
		(make-op :action 'take-pe-1408
			:add-list '(pe-1408))
		(make-op :action 'take-pe-1410
			:add-list '(pe-1410))
		(make-op :action 'take-pe-1430
			:add-list '(pe-1430))
		(make-op :action 'take-pe-1440
			:add-list '(pe-1440))
		(make-op :action 'take-pe-1444
			:add-list '(pe-1444))
		(make-op :action 'take-pe-1445
			:add-list '(pe-1445))
		(make-op :action 'take-pe-1446
			:add-list '(pe-1446))
		(make-op :action 'take-pe-1480
			:add-list '(pe-1480))
		(make-op :action 'take-pe-1490
			:add-list '(pe-1490))
		(make-op :action 'take-pe-1500
			:add-list '(pe-1500))
		(make-op :action 'take-pe-1505
			:add-list '(pe-1505))
		(make-op :action 'take-pe-1506
			:add-list '(pe-1506))
		(make-op :action 'take-pe-1507
			:add-list '(pe-1507))
		(make-op :action 'take-pe-1508
			:add-list '(pe-1508))
		(make-op :action 'take-pe-1509
			:add-list '(pe-1509))
		(make-op :action 'take-pe-1510
			:add-list '(pe-1510))
		(make-op :action 'take-pe-1511
			:add-list '(pe-1511))
		(make-op :action 'take-pe-1512
			:add-list '(pe-1512))
		(make-op :action 'take-pe-1513
			:add-list '(pe-1513))
		(make-op :action 'take-pe-1514
			:add-list '(pe-1514))
		(make-op :action 'take-pe-1515
			:add-list '(pe-1515))
		(make-op :action 'take-pe-1520
			:add-list '(pe-1520))
		(make-op :action 'take-pe-1523
			:add-list '(pe-1523))
		(make-op :action 'take-pe-1527
			:add-list '(pe-1527))
		(make-op :action 'take-pe-1528
			:add-list '(pe-1528))
		(make-op :action 'take-pe-1529
			:add-list '(pe-1529))
		(make-op :action 'take-pe-1530
			:add-list '(pe-1530))
		(make-op :action 'take-pe-1532
			:add-list '(pe-1532))
		(make-op :action 'take-pe-1535
			:add-list '(pe-1535))
		(make-op :action 'take-pe-1536
			:add-list '(pe-1536))
		(make-op :action 'take-pe-1538
			:add-list '(pe-1538))
		(make-op :action 'take-pe-1543
			:add-list '(pe-1543))
		(make-op :action 'take-pe-1544
			:add-list '(pe-1544))
		(make-op :action 'take-pe-1547
			:add-list '(pe-1547))
		(make-op :action 'take-pe-1548
			:preconds '(pe-1547 pe-1549)
			:add-list '(pe-1548))
		(make-op :action 'take-pe-1549
			:add-list '(pe-1549))
		(make-op :action 'take-pe-1550
			:add-list '(pe-1550))
		(make-op :action 'take-pe-1570
			:add-list '(pe-1570))
		(make-op :action 'take-pe-1580
			:add-list '(pe-1580))
		(make-op :action 'take-pe-1600
			:add-list '(pe-1600))
		(make-op :action 'take-pe-1605
			:add-list '(pe-1605))
		(make-op :action 'take-pe-1615
			:add-list '(pe-1615))
		(make-op :action 'take-pe-1625
			:add-list '(pe-1625))
		(make-op :action 'take-pe-1630
			:add-list '(pe-1630))
		(make-op :action 'take-pe-1635
			:add-list '(pe-1635))
		(make-op :action 'take-pe-1655
			:add-list '(pe-1655))
		(make-op :action 'take-pe-1670
			:add-list '(pe-1670))
		(make-op :action 'take-pe-1671
			:add-list '(pe-1671))
		(make-op :action 'take-pe-1680
			:add-list '(pe-1680))
		(make-op :action 'take-pe-1700
			:add-list '(pe-1700))
		(make-op :action 'take-pe-1701
			:add-list '(pe-1701))
		(make-op :action 'take-pe-1702
			:add-list '(pe-1702))
		(make-op :action 'take-pe-1705
			:add-list '(pe-1705))
		(make-op :action 'take-pe-1710
			:add-list '(pe-1710))
		(make-op :action 'take-pe-1711
			:preconds '(pe-1710)
			:add-list '(pe-1711))
		(make-op :action 'take-pe-1720
			:add-list '(pe-1720))
		(make-op :action 'take-pe-1745
			:add-list '(pe-1745))
		(make-op :action 'take-pe-1746
			:add-list '(pe-1746))
		(make-op :action 'take-pe-1760
			:add-list '(pe-1760))
		(make-op :action 'take-pe-1765
			:add-list '(pe-1765))
		(make-op :action 'take-pe-1770
			:add-list '(pe-1770))
		(make-op :action 'take-pe-1800
			:add-list '(pe-1800))
		(make-op :action 'take-pe-1900
			:add-list '(pe-1900))
		(make-op :action 'take-pe-1905
			:add-list '(pe-1905))
		(make-op :action 'take-pe-1906
			:add-list '(pe-1906))
		(make-op :action 'take-pe-1910
			:add-list '(pe-1910))
		(make-op :action 'take-pe-1915
			:add-list '(pe-1915))
		(make-op :action 'take-pe-2000
			:add-list '(pe-2000))
		(make-op :action 'take-pe-2005
			:add-list '(pe-2005))
		(make-op :action 'take-pe-2010
			:add-list '(pe-2010))
		(make-op :action 'take-pe-2020
			:add-list '(pe-2020))
		(make-op :action 'take-pe-2030
			:add-list '(pe-2030))
		(make-op :action 'take-pe-2031
			:preconds '(pe-2030 pe-2030)
			:add-list '(pe-2031))
		(make-op :action 'take-pe-2040
			:add-list '(pe-2040))
		(make-op :action 'take-pe-2041
			:preconds '(pe-2040 pe-2040)
			:add-list '(pe-2041))
		(make-op :action 'take-pe-2050
			:add-list '(pe-2050))
		(make-op :action 'take-pe-2060
			:add-list '(pe-2060))
		(make-op :action 'take-pe-2061
			:preconds '(pe-2060)
			:add-list '(pe-2061))
		(make-op :action 'take-pe-2062
			:preconds '(pe-2060 pe-2061)
			:add-list '(pe-2062))
		(make-op :action 'take-pe-2063
			:preconds '(pe-2060 pe-2061 pe-2062)
			:add-list '(pe-2063))
		(make-op :action 'take-pe-2070
			:add-list '(pe-2070))
		(make-op :action 'take-pe-2080
			:add-list '(pe-2080))
		(make-op :action 'take-pe-2090
			:add-list '(pe-2090))
		(make-op :action 'take-pe-2091
			:preconds '(pe-2090 pe-2090)
			:add-list '(pe-2091))
		(make-op :action 'take-pe-2092
			:preconds '(pe-2091)
			:add-list '(pe-2092))
		(make-op :action 'take-pe-2093
			:preconds '(pe-2092)
			:add-list '(pe-2093))
		(make-op :action 'take-pe-2100
			:add-list '(pe-2100))
		(make-op :action 'take-pe-2110
			:add-list '(pe-2110))
		(make-op :action 'take-pe-2120
			:add-list '(pe-2120))
		(make-op :action 'take-pe-2121
			:preconds '(pe-2120)
			:add-list '(pe-2121))
		(make-op :action 'take-pe-2122
			:preconds '(pe-2120 pe-2121)
			:add-list '(pe-2122))
		(make-op :action 'take-pe-2123
			:preconds '(pe-2120 pe-2121 pe-2122)
			:add-list '(pe-2123))
		(make-op :action 'take-pe-2130
			:add-list '(pe-2130))
		(make-op :action 'take-pe-2131
			:preconds '(pe-2130)
			:add-list '(pe-2131))
		(make-op :action 'take-pe-2132
			:preconds '(pe-2130 pe-2131)
			:add-list '(pe-2132))
		(make-op :action 'take-pe-2133
			:preconds '(pe-2130 pe-2131 pe-2132)
			:add-list '(pe-2133))
		(make-op :action 'take-pe-2988
			:add-list '(pe-2988))
		(make-op :action 'take-pe-2999
			:add-list '(pe-2999))
		(make-op :action 'take-pe-3000
			:add-list '(pe-3000))
		(make-op :action 'take-pe-4000
			:add-list '(pe-4000))
		(make-op :action 'take-pe-4050
			:add-list '(pe-4050))
		(make-op :action 'take-pe-4200
			:add-list '(pe-4200))
		(make-op :action 'take-phys-1010
			:add-list '(phys-1010))
		(make-op :action 'take-phys-1015
			:add-list '(phys-1015))
		(make-op :action 'take-phys-1020
			:add-list '(phys-1020))
		(make-op :action 'take-phys-1040
			:add-list '(phys-1040))
		(make-op :action 'take-phys-1050
			:add-list '(phys-1050))
		(make-op :action 'take-phys-1080
			:add-list '(phys-1080))
		(make-op :action 'take-phys-1100
			:add-list '(phys-1100))
		(make-op :action 'take-phys-1200
			:add-list '(phys-1200))
		(make-op :action 'take-phys-1800
			:preconds '(math-1050 math-1060)
			:add-list '(phys-1800))
		(make-op :action 'take-phys-2010
			:preconds '(math-1210)
			:add-list '(phys-2010))
		(make-op :action 'take-phys-2015
			:add-list '(phys-2015))
		(make-op :action 'take-phys-2020
			:add-list '(phys-2020))
		(make-op :action 'take-phys-2025
			:add-list '(phys-2025))
		(make-op :action 'take-phys-2110
			:preconds '(math-1060 math-1100 math-1210)
			:add-list '(phys-2110))
		(make-op :action 'take-phys-2120
			:preconds '(phys-2110 math-1060 math-1100 math-1210)
			:add-list '(phys-2120))
		(make-op :action 'take-phys-2210
			:preconds '(math-1210)
			:add-list '(phys-2210))
		(make-op :action 'take-phys-2215
			:preconds '(phys-2210 phys-2310 phys-2210 phys-2310)
			:add-list '(phys-2215))
		(make-op :action 'take-phys-2220
			:preconds '(math-1220 phys-2210 phys-2310)
			:add-list '(phys-2220))
		(make-op :action 'take-phys-2225
			:preconds '(phys-2220 phys-2320 phys-2320)
			:add-list '(phys-2225))
		(make-op :action 'take-phys-2310
			:preconds '(math-1210)
			:add-list '(phys-2310))
		(make-op :action 'take-phys-2320
			:preconds '(math-1220 phys-2310 phys-2210)
			:add-list '(phys-2320))
		(make-op :action 'take-phys-2400
			:add-list '(phys-2400))
		(make-op :action 'take-phys-2500
			:preconds '(phys-1800 phys-2110 phys-2210 phys-2310)
			:add-list '(phys-2500))
		(make-op :action 'take-phys-2710
			:preconds '(math-1220 phys-2220 phys-2320)
			:add-list '(phys-2710))
		(make-op :action 'take-phys-2977
			:add-list '(phys-2977))
		(make-op :action 'take-phys-2988
			:add-list '(phys-2988))
		(make-op :action 'take-phys-3010
			:add-list '(phys-3010))
		(make-op :action 'take-phys-3020
			:add-list '(phys-3020))
		(make-op :action 'take-phys-3030
			:add-list '(phys-3030))
		(make-op :action 'take-phys-3040
			:add-list '(phys-3040))
		(make-op :action 'take-phys-3150
			:add-list '(phys-3150))
		(make-op :action 'take-phys-3500
			:preconds '(phys-2710)
			:add-list '(phys-3500))
		(make-op :action 'take-phys-3550
			:preconds '(phys-2220 phys-2320 math-2210 math-2250 math-2280)
			:add-list '(phys-3550))
		(make-op :action 'take-phys-3600
			:preconds '(phys-2710 phys-3550 math-2210 math-2250 math-2280)
			:add-list '(phys-3600))
		(make-op :action 'take-phys-3700
			:preconds '(phys-2710)
			:add-list '(phys-3700))
		(make-op :action 'take-phys-3710
			:preconds '(math-1220 phys-2220 phys-2320)
			:add-list '(phys-3710))
		(make-op :action 'take-phys-3750
			:preconds '(math-2210 math-2250 math-2280 phys-2710 phys-3550)
			:add-list '(phys-3750))
		(make-op :action 'take-phys-3870
			:preconds '(phys-2500 phys-2710)
			:add-list '(phys-3870))
		(make-op :action 'take-phys-3880
			:preconds '(phys-3870)
			:add-list '(phys-3880))
		(make-op :action 'take-phys-3900
			:add-list '(phys-3900))
		(make-op :action 'take-phys-4020
			:add-list '(phys-4020))
		(make-op :action 'take-phys-4250
			:preconds '(phys-2710)
			:add-list '(phys-4250))
		(make-op :action 'take-phys-4600
			:preconds '(phys-3600 phys-3750)
			:add-list '(phys-4600))
		(make-op :action 'take-phys-4650
			:preconds '(phys-2710 math-2210 math-2250 math-2280 ece-3870 phys-6650)
			:add-list '(phys-4650))
		(make-op :action 'take-phys-4680
			:preconds '(phys-4650 phys-6650 phys-6680)
			:add-list '(phys-4680))
		(make-op :action 'take-phys-4700
			:preconds '(phys-3600 phys-3750)
			:add-list '(phys-4700))
		(make-op :action 'take-phys-4710
			:preconds '(phys-4700 phys-4700)
			:add-list '(phys-4710))
		(make-op :action 'take-phys-4900
			:preconds '(phys-2710)
			:add-list '(phys-4900))
		(make-op :action 'take-phys-5210
			:preconds '(phys-3600 ece-3870 ece-5210)
			:add-list '(phys-5210))
		(make-op :action 'take-phys-5250
			:preconds '(phys-4650 phys-4600 ece-5800 ece-5250)
			:add-list '(phys-5250))
		(make-op :action 'take-phys-5330
			:preconds '(phys-4600 phys-6330)
			:add-list '(phys-5330))
		(make-op :action 'take-phys-5340
			:add-list '(phys-5340))
		(make-op :action 'take-phys-5350
			:preconds '(phys-5340 phys-5340)
			:add-list '(phys-5350))
		(make-op :action 'take-phys-5500
			:add-list '(phys-5500))
		(make-op :action 'take-phys-5530
			:preconds '(phys-3700 phys-4600 phys-4710 phys-6530)
			:add-list '(phys-5530))
		(make-op :action 'take-phys-5700
			:preconds '(phys-2210 phys-2310 phys-2215 phys-2220 phys-2320 phys-2225 beng-5700 ece-5700 mae-5700)
			:add-list '(phys-5700))
		(make-op :action 'take-phys-5710
			:preconds '(beng-5700 ece-5700 mae-5700 phys-5700 beng-5710 ece-5710)
			:add-list '(phys-5710))
		(make-op :action 'take-phys-5800
			:add-list '(phys-5800))
		(make-op :action 'take-phys-5910
			:preconds '(phys-3710 phys-4600 phys-6910)
			:add-list '(phys-5910))
		(make-op :action 'take-psc-1050
			:add-list '(psc-1050))
		(make-op :action 'take-psc-1800
			:add-list '(psc-1800))
		(make-op :action 'take-psc-2000
			:add-list '(psc-2000))
		(make-op :action 'take-psc-2010
			:add-list '(psc-2010))
		(make-op :action 'take-psc-2030
			:add-list '(psc-2030))
		(make-op :action 'take-psc-2040
			:add-list '(psc-2040))
		(make-op :action 'take-psc-2200
			:add-list '(psc-2200))
		(make-op :action 'take-psc-2250
			:add-list '(psc-2250))
		(make-op :action 'take-psc-2600
			:add-list '(psc-2600))
		(make-op :action 'take-psc-2620
			:add-list '(psc-2620))
		(make-op :action 'take-psc-2700
			:add-list '(psc-2700))
		(make-op :action 'take-psc-2900
			:add-list '(psc-2900))
		(make-op :action 'take-psc-3000
			:preconds '(chem-1110 math-1050)
			:add-list '(psc-3000))
		(make-op :action 'take-psc-3100
			:add-list '(psc-3100))
		(make-op :action 'take-psc-3200
			:add-list '(psc-3200))
		(make-op :action 'take-psc-3300
			:preconds '(psc-2620 psc-2600)
			:add-list '(psc-3300))
		(make-op :action 'take-psc-3400
			:preconds '(psc-2620)
			:add-list '(psc-3400))
		(make-op :action 'take-psc-3430
			:add-list '(psc-3430))
		(make-op :action 'take-psc-3500
			:preconds '(biol-1010 biol-1610)
			:add-list '(psc-3500))
		(make-op :action 'take-psc-3600
			:add-list '(psc-3600))
		(make-op :action 'take-psc-3700
			:preconds '(biol-1610)
			:add-list '(psc-3700))
		(make-op :action 'take-psc-3800
			:add-list '(psc-3800))
		(make-op :action 'take-psc-3810
			:preconds '(biol-1010 biol-1610)
			:add-list '(psc-3810))
		(make-op :action 'take-psc-4050
			:add-list '(psc-4050))
		(make-op :action 'take-psc-4070
			:preconds '(psc-3800 psc-3000 psc-3500 psc-6070)
			:add-list '(psc-4070))
		(make-op :action 'take-psc-4080
			:preconds '(psc-6080)
			:add-list '(psc-4080))
		(make-op :action 'take-psc-4100
			:preconds '(psc-3810 psc-6100)
			:add-list '(psc-4100))
		(make-op :action 'take-psc-4123
			:preconds '(psc-6123)
			:add-list '(psc-4123))
		(make-op :action 'take-psc-4150
			:preconds '(psc-6150)
			:add-list '(psc-4150))
		(make-op :action 'take-psc-4200
			:preconds '(biol-1010 biol-1610)
			:add-list '(psc-4200))
		(make-op :action 'take-psc-4250
			:add-list '(psc-4250))
		(make-op :action 'take-psc-4280
			:add-list '(psc-4280))
		(make-op :action 'take-psc-4301
			:add-list '(psc-4301))
		(make-op :action 'take-psc-4302
			:add-list '(psc-4302))
		(make-op :action 'take-psc-4310
			:add-list '(psc-4310))
		(make-op :action 'take-psc-4320
			:add-list '(psc-4320))
		(make-op :action 'take-psc-4400
			:preconds '(biol-1010 biol-1610)
			:add-list '(psc-4400))
		(make-op :action 'take-psc-4420
			:preconds '(psc-2620 psc-6420)
			:add-list '(psc-4420))
		(make-op :action 'take-psc-4500
			:preconds '(psc-3000)
			:add-list '(psc-4500))
		(make-op :action 'take-psc-4510
			:preconds '(psc-3500)
			:add-list '(psc-4510))
		(make-op :action 'take-psc-4550
			:add-list '(psc-4550))
		(make-op :action 'take-psc-4700
			:preconds '(psc-3000)
			:add-list '(psc-4700))
		(make-op :action 'take-psc-4740
			:preconds '(psc-2000 psc-4123 psc-6123 psc-4810)
			:add-list '(psc-4740))
		(make-op :action 'take-psc-4750
			:preconds '(geo-1110 math-1210 math-1220 psc-6750)
			:add-list '(psc-4750))
		(make-op :action 'take-psc-4810
			:preconds '(psc-6810)
			:add-list '(psc-4810))
		(make-op :action 'take-psc-4820
			:preconds '(geo-5680)
			:add-list '(psc-4820))
		(make-op :action 'take-psc-4830
			:preconds '(math-1210 psc-2000)
			:add-list '(psc-4830))
		(make-op :action 'take-psc-4840
			:preconds '(math-2210 phys-2210 psc-2000)
			:add-list '(psc-4840))
		(make-op :action 'take-psc-4850
			:preconds '(math-2210 phys-2210 psc-2000)
			:add-list '(psc-4850))
		(make-op :action 'take-psc-4860
			:preconds '(math-2210 phys-2210 math-2250)
			:add-list '(psc-4860))
		(make-op :action 'take-psc-4870
			:add-list '(psc-4870))
		(make-op :action 'take-psc-4880
			:preconds '(math-1210 math-1220 psc-6880)
			:add-list '(psc-4880))
		(make-op :action 'take-psc-4890
			:add-list '(psc-4890))
		(make-op :action 'take-psc-4900
			:add-list '(psc-4900))
		(make-op :action 'take-psc-4970
			:add-list '(psc-4970))
		(make-op :action 'take-psc-5000
			:preconds '(psc-6000)
			:add-list '(psc-5000))
		(make-op :action 'take-psc-5003
			:preconds '(cee-2450 cee-5003 psc-6003)
			:add-list '(psc-5003))
		(make-op :action 'take-psc-5050
			:preconds '(chem-1110 math-1050 psc-6050)
			:add-list '(psc-5050))
		(make-op :action 'take-psc-5090
			:preconds '(laep-5090 psc-6090)
			:add-list '(psc-5090))
		(make-op :action 'take-psc-5130
			:preconds '(psc-3000 psc-6130)
			:add-list '(psc-5130))
		(make-op :action 'take-psc-5160
			:preconds '(advs-5160 biol-5160 ndfs-5160)
			:add-list '(psc-5160))
		(make-op :action 'take-psc-5260
			:preconds '(chem-3700 chem-5710 biol-3060 advs-5260 biol-5260 ndfs-5260)
			:add-list '(psc-5260))
		(make-op :action 'take-psc-5270
			:preconds '(biol-4400 psc-3500 psc-6270)
			:add-list '(psc-5270))
		(make-op :action 'take-psc-5280
			:preconds '(biol-4400 psc-6280)
			:add-list '(psc-5280))
		(make-op :action 'take-psc-5300
			:preconds '(biol-3060)
			:add-list '(psc-5300))
		(make-op :action 'take-psc-5310
			:preconds '(biol-1610 biol-1620 chem-2300 chem-2310 psc-3000 biol-5310)
			:add-list '(psc-5310))
		(make-op :action 'take-psc-5350
			:preconds '(chem-1110 psc-3000 wild-5350 psc-6350)
			:add-list '(psc-5350))
		(make-op :action 'take-psc-5410
			:add-list '(psc-5410))
		(make-op :action 'take-psc-5430
			:preconds '(psc-3500 biol-4400 psc-6430)
			:add-list '(psc-5430))
		(make-op :action 'take-psc-5500
			:preconds '(psc-6500)
			:add-list '(psc-5500))
		(make-op :action 'take-psc-5530
			:preconds '(psc-3000 chem-1110 chem-1210 psc-6530)
			:add-list '(psc-5530))
		(make-op :action 'take-psc-5550
			:preconds '(psc-6550)
			:add-list '(psc-5550))
		(make-op :action 'take-psc-5560
			:preconds '(psc-5050 psc-6050 psc-5530 psc-6530 psc-6560)
			:add-list '(psc-5560))
		(make-op :action 'take-psc-5620
			:preconds '(chem-1210 cee-5620)
			:add-list '(psc-5620))
		(make-op :action 'take-psc-5670
			:preconds '(psc-6670)
			:add-list '(psc-5670))
		(make-op :action 'take-psc-5680
			:preconds '(geo-3600 wats-3600 geo-5680 wats-5680 psc-6680)
			:add-list '(psc-5680))
		(make-op :action 'take-psc-5700
			:preconds '(biol-1620)
			:add-list '(psc-5700))
		(make-op :action 'take-psc-5740
			:preconds '(psc-5130)
			:add-list '(psc-5740))
		(make-op :action 'take-psc-5750
			:preconds '(biol-3060)
			:add-list '(psc-5750))
		(make-op :action 'take-psc-5777
			:add-list '(psc-5777))
		(make-op :action 'take-tepl-1110
			:add-list '(tepl-1110))
		(make-op :action 'take-tepl-1120
			:add-list '(tepl-1120))
		(make-op :action 'take-tepl-1210
			:preconds '(tepl-1120)
			:add-list '(tepl-1210))
		(make-op :action 'take-tepl-1220
			:preconds '(tepl-1210)
			:add-list '(tepl-1220))
		(make-op :action 'take-tepl-1310
			:add-list '(tepl-1310))
		(make-op :action 'take-tepl-1320
			:add-list '(tepl-1320))
		(make-op :action 'take-tepl-1410
			:add-list '(tepl-1410))
		(make-op :action 'take-tepl-1420
			:add-list '(tepl-1420))
		(make-op :action 'take-tepl-2310
			:preconds '(tepl-1220)
			:add-list '(tepl-2310))
		(make-op :action 'take-tepl-2320
			:preconds '(tepl-2310)
			:add-list '(tepl-2320))
		(make-op :action 'take-tepl-2410
			:preconds '(tepl-2320)
			:add-list '(tepl-2410))
		(make-op :action 'take-tepl-2420
			:preconds '(tepl-2410)
			:add-list '(tepl-2420))
		(make-op :action 'take-pols-1010
			:add-list '(pols-1010))
		(make-op :action 'take-pols-1100
			:add-list '(pols-1100))
		(make-op :action 'take-pols-1200
			:add-list '(pols-1200))
		(make-op :action 'take-pols-2100
			:add-list '(pols-2100))
		(make-op :action 'take-pols-2110
			:add-list '(pols-2110))
		(make-op :action 'take-pols-2200
			:add-list '(pols-2200))
		(make-op :action 'take-pols-2300
			:add-list '(pols-2300))
		(make-op :action 'take-pols-2400
			:add-list '(pols-2400))
		(make-op :action 'take-pols-2500
			:add-list '(pols-2500))
		(make-op :action 'take-pols-2971
			:add-list '(pols-2971))
		(make-op :action 'take-pols-2972
			:add-list '(pols-2972))
		(make-op :action 'take-pols-2973
			:add-list '(pols-2973))
		(make-op :action 'take-pols-2974
			:add-list '(pols-2974))
		(make-op :action 'take-pols-2975
			:add-list '(pols-2975))
		(make-op :action 'take-pols-2988
			:add-list '(pols-2988))
		(make-op :action 'take-pols-2999
			:add-list '(pols-2999))
		(make-op :action 'take-pols-3000
			:add-list '(pols-3000))
		(make-op :action 'take-pols-3100
			:add-list '(pols-3100))
		(make-op :action 'take-pols-3110
			:add-list '(pols-3110))
		(make-op :action 'take-pols-3115
			:add-list '(pols-3115))
		(make-op :action 'take-pols-3120
			:add-list '(pols-3120))
		(make-op :action 'take-pols-3130
			:add-list '(pols-3130))
		(make-op :action 'take-pols-3140
			:add-list '(pols-3140))
		(make-op :action 'take-pols-3150
			:add-list '(pols-3150))
		(make-op :action 'take-pols-3170
			:preconds '(ecn-3170)
			:add-list '(pols-3170))
		(make-op :action 'take-pols-3180
			:preconds '(pols-1100)
			:add-list '(pols-3180))
		(make-op :action 'take-pols-3190
			:add-list '(pols-3190))
		(make-op :action 'take-pols-3200
			:add-list '(pols-3200))
		(make-op :action 'take-pols-3210
			:add-list '(pols-3210))
		(make-op :action 'take-pols-3220
			:add-list '(pols-3220))
		(make-op :action 'take-pols-3230
			:add-list '(pols-3230))
		(make-op :action 'take-pols-3240
			:add-list '(pols-3240))
		(make-op :action 'take-pols-3250
			:add-list '(pols-3250))
		(make-op :action 'take-pols-3260
			:preconds '(soc-3260)
			:add-list '(pols-3260))
		(make-op :action 'take-pols-3270
			:add-list '(pols-3270))
		(make-op :action 'take-pols-3310
			:add-list '(pols-3310))
		(make-op :action 'take-pols-3320
			:add-list '(pols-3320))
		(make-op :action 'take-pols-3400
			:add-list '(pols-3400))
		(make-op :action 'take-pols-3410
			:preconds '(pols-2100 pols-2200)
			:add-list '(pols-3410))
		(make-op :action 'take-pols-3430
			:preconds '(engl-2010 pols-2100 pols-2200 pols-2300 pols-2400 geog-3430)
			:add-list '(pols-3430))
		(make-op :action 'take-pols-3700
			:add-list '(pols-3700))
		(make-op :action 'take-pols-3810
			:add-list '(pols-3810))
		(make-op :action 'take-pols-4120
			:add-list '(pols-4120))
		(make-op :action 'take-pols-4130
			:preconds '(pols-1100)
			:add-list '(pols-4130))
		(make-op :action 'take-pols-4140
			:preconds '(pols-1100)
			:add-list '(pols-4140))
		(make-op :action 'take-pols-4150
			:add-list '(pols-4150))
		(make-op :action 'take-pols-4160
			:add-list '(pols-4160))
		(make-op :action 'take-pols-4210
			:add-list '(pols-4210))
		(make-op :action 'take-pols-4220
			:add-list '(pols-4220))
		(make-op :action 'take-pols-4230
			:add-list '(pols-4230))
		(make-op :action 'take-pols-4260
			:add-list '(pols-4260))
		(make-op :action 'take-pols-4270
			:add-list '(pols-4270))
		(make-op :action 'take-pols-4280
			:add-list '(pols-4280))
		(make-op :action 'take-pols-4310
			:add-list '(pols-4310))
		(make-op :action 'take-pols-4320
			:add-list '(pols-4320))
		(make-op :action 'take-pols-4330
			:add-list '(pols-4330))
		(make-op :action 'take-pols-4350
			:add-list '(pols-4350))
		(make-op :action 'take-pols-4360
			:preconds '(pols-2300)
			:add-list '(pols-4360))
		(make-op :action 'take-pols-4370
			:preconds '(pols-2300)
			:add-list '(pols-4370))
		(make-op :action 'take-pols-4410
			:add-list '(pols-4410))
		(make-op :action 'take-pols-4450
			:add-list '(pols-4450))
		(make-op :action 'take-pols-4460
			:preconds '(engl-2010)
			:add-list '(pols-4460))
		(make-op :action 'take-pols-4462
			:preconds '(pols-1100 pols-2100)
			:add-list '(pols-4462))
		(make-op :action 'take-pols-4463
			:preconds '(pols-1100 pols-2100)
			:add-list '(pols-4463))
		(make-op :action 'take-pols-4464
			:add-list '(pols-4464))
		(make-op :action 'take-pols-4470
			:add-list '(pols-4470))
		(make-op :action 'take-pols-4500
			:preconds '(pols-1100 pols-2100 pols-2200 pols-2400 pols-2500)
			:add-list '(pols-4500))
		(make-op :action 'take-pols-4800
			:add-list '(pols-4800))
		(make-op :action 'take-pols-4820
			:add-list '(pols-4820))
		(make-op :action 'take-pols-4840
			:add-list '(pols-4840))
		(make-op :action 'take-pols-4850
			:preconds '(cmst-1330 cmst-2110 pols-1100 pols-2100 pols-2200 pols-2400 iogp-4850)
			:add-list '(pols-4850))
		(make-op :action 'take-pols-4890
			:add-list '(pols-4890))
		(make-op :action 'take-pols-4910
			:add-list '(pols-4910))
		(make-op :action 'take-pols-4990
			:add-list '(pols-4990))
		(make-op :action 'take-pols-5000
			:preconds '(pols-3000)
			:add-list '(pols-5000))
		(make-op :action 'take-pols-5100
			:add-list '(pols-5100))
		(make-op :action 'take-pols-5110
			:add-list '(pols-5110))
		(make-op :action 'take-pols-5120
			:preconds '(apec-2010 ecn-2010)
			:add-list '(pols-5120))
		(make-op :action 'take-pols-5130
			:add-list '(pols-5130))
		(make-op :action 'take-pols-5140
			:add-list '(pols-5140))
		(make-op :action 'take-pols-5200
			:add-list '(pols-5200))
		(make-op :action 'take-pols-5210
			:add-list '(pols-5210))
		(make-op :action 'take-pols-5270
			:add-list '(pols-5270))
		(make-op :action 'take-pols-5290
			:add-list '(pols-5290))
		(make-op :action 'take-pols-5300
			:add-list '(pols-5300))
		(make-op :action 'take-pols-5350
			:add-list '(pols-5350))
		(make-op :action 'take-pols-5400
			:add-list '(pols-5400))
		(make-op :action 'take-pols-5420
			:preconds '(jcom-5420 pols-6420)
			:add-list '(pols-5420))
		(make-op :action 'take-pols-5480
			:add-list '(pols-5480))
		(make-op :action 'take-pols-5770
			:preconds '(pols-2100 pols-2200)
			:add-list '(pols-5770))
		(make-op :action 'take-pols-5890
			:add-list '(pols-5890))
		(make-op :action 'take-pols-5900
			:add-list '(pols-5900))
		(make-op :action 'take-pols-5910
			:add-list '(pols-5910))
		(make-op :action 'take-pols-5920
			:add-list '(pols-5920))
		(make-op :action 'take-pols-5930
			:add-list '(pols-5930))
		(make-op :action 'take-pols-5940
			:add-list '(pols-5940))
		(make-op :action 'take-pols-5950
			:add-list '(pols-5950))
		(make-op :action 'take-pols-5960
			:add-list '(pols-5960))
		(make-op :action 'take-port-1010
			:add-list '(port-1010))
		(make-op :action 'take-port-1020
			:preconds '(port-1010)
			:add-list '(port-1020))
		(make-op :action 'take-port-1050
			:add-list '(port-1050))
		(make-op :action 'take-port-1800
			:add-list '(port-1800))
		(make-op :action 'take-port-1820
			:add-list '(port-1820))
		(make-op :action 'take-port-2010
			:preconds '(port-1020)
			:add-list '(port-2010))
		(make-op :action 'take-port-2020
			:preconds '(port-2010)
			:add-list '(port-2020))
		(make-op :action 'take-port-2800
			:add-list '(port-2800))
		(make-op :action 'take-port-2820
			:add-list '(port-2820))
		(make-op :action 'take-port-2880
			:add-list '(port-2880))
		(make-op :action 'take-port-3000
			:preconds '(port-2020)
			:add-list '(port-3000))
		(make-op :action 'take-port-3040
			:preconds '(port-2020)
			:add-list '(port-3040))
		(make-op :action 'take-port-3116
			:add-list '(port-3116))
		(make-op :action 'take-port-3117
			:add-list '(port-3117))
		(make-op :action 'take-port-3118
			:add-list '(port-3118))
		(make-op :action 'take-port-3400
			:preconds '(port-3040)
			:add-list '(port-3400))
		(make-op :action 'take-port-3510
			:preconds '(port-3040)
			:add-list '(port-3510))
		(make-op :action 'take-port-3540
			:preconds '(port-3040)
			:add-list '(port-3540))
		(make-op :action 'take-port-3570
			:preconds '(port-3040)
			:add-list '(port-3570))
		(make-op :action 'take-port-3630
			:preconds '(port-3040)
			:add-list '(port-3630))
		(make-op :action 'take-port-3700
			:preconds '(port-3040)
			:add-list '(port-3700))
		(make-op :action 'take-port-3800
			:preconds '(port-2020)
			:add-list '(port-3800))
		(make-op :action 'take-port-3820
			:preconds '(port-2020)
			:add-list '(port-3820))
		(make-op :action 'take-port-4000
			:preconds '(port-3040)
			:add-list '(port-4000))
		(make-op :action 'take-port-4100
			:preconds '(port-3040)
			:add-list '(port-4100))
		(make-op :action 'take-port-4200
			:preconds '(port-3040)
			:add-list '(port-4200))
		(make-op :action 'take-port-4800
			:preconds '(port-2020)
			:add-list '(port-4800))
		(make-op :action 'take-port-4880
			:add-list '(port-4880))
		(make-op :action 'take-port-4920
			:add-list '(port-4920))
		(make-op :action 'take-psy-1010
			:add-list '(psy-1010))
		(make-op :action 'take-psy-1100
			:preconds '(psy-1010)
			:add-list '(psy-1100))
		(make-op :action 'take-psy-1400
			:preconds '(psy-1010 psy-1410)
			:add-list '(psy-1400))
		(make-op :action 'take-psy-1410
			:preconds '(psy-1400 psy-1010)
			:add-list '(psy-1410))
		(make-op :action 'take-psy-2010
			:preconds '(psy-1010)
			:add-list '(psy-2010))
		(make-op :action 'take-psy-2100
			:preconds '(psy-1010)
			:add-list '(psy-2100))
		(make-op :action 'take-psy-2250
			:add-list '(psy-2250))
		(make-op :action 'take-psy-2300
			:preconds '(psy-1010)
			:add-list '(psy-2300))
		(make-op :action 'take-psy-2400
			:preconds '(psy-1010 psy-1400)
			:add-list '(psy-2400))
		(make-op :action 'take-psy-3000
			:add-list '(psy-3000))
		(make-op :action 'take-psy-3010
			:preconds '(stat-1040 stat-1045 stat-2000)
			:add-list '(psy-3010))
		(make-op :action 'take-psy-3020
			:preconds '(psy-2010)
			:add-list '(psy-3020))
		(make-op :action 'take-psy-3110
			:preconds '(psy-1010)
			:add-list '(psy-3110))
		(make-op :action 'take-psy-3120
			:preconds '(psy-1010)
			:add-list '(psy-3120))
		(make-op :action 'take-psy-3400
			:preconds '(psy-1400 psy-1400 psy-1410)
			:add-list '(psy-3400))
		(make-op :action 'take-psy-3450
			:preconds '(psy-1010)
			:add-list '(psy-3450))
		(make-op :action 'take-psy-3460
			:preconds '(psy-1010)
			:add-list '(psy-3460))
		(make-op :action 'take-psy-3500
			:preconds '(psy-1010 engl-2010)
			:add-list '(psy-3500))
		(make-op :action 'take-psy-3510
			:preconds '(psy-1010)
			:add-list '(psy-3510))
		(make-op :action 'take-psy-3700
			:preconds '(hdfs-3700)
			:add-list '(psy-3700))
		(make-op :action 'take-psy-4000
			:preconds '(kin-4000)
			:add-list '(psy-4000))
		(make-op :action 'take-psy-4020
			:preconds '(psy-2010)
			:add-list '(psy-4020))
		(make-op :action 'take-psy-4210
			:preconds '(psy-1010)
			:add-list '(psy-4210))
		(make-op :action 'take-psy-4230
			:add-list '(psy-4230))
		(make-op :action 'take-psy-4240
			:preconds '(psy-1010)
			:add-list '(psy-4240))
		(make-op :action 'take-psy-4250
			:add-list '(psy-4250))
		(make-op :action 'take-psy-4420
			:preconds '(psy-1010)
			:add-list '(psy-4420))
		(make-op :action 'take-psy-4460
			:preconds '(psy-1010 psy-3460 biol-1610)
			:add-list '(psy-4460))
		(make-op :action 'take-psy-4910
			:add-list '(psy-4910))
		(make-op :action 'take-psy-4920
			:add-list '(psy-4920))
		(make-op :action 'take-psy-4930
			:preconds '(hdfs-4930)
			:add-list '(psy-4930))
		(make-op :action 'take-psy-4940
			:add-list '(psy-4940))
		(make-op :action 'take-psy-4950
			:preconds '(psy-2010)
			:add-list '(psy-4950))
		(make-op :action 'take-psy-4960
			:preconds '(psy-1010)
			:add-list '(psy-4960))
		(make-op :action 'take-psy-5050
			:preconds '(kin-5050 psy-6050)
			:add-list '(psy-5050))
		(make-op :action 'take-psy-5100
			:preconds '(psy-6100)
			:add-list '(psy-5100))
		(make-op :action 'take-psy-5200
			:add-list '(psy-5200))
		(make-op :action 'take-psy-5280
			:preconds '(psy-1400 psy-1410)
			:add-list '(psy-5280))
		(make-op :action 'take-psy-5330
			:preconds '(psy-1010 psy-3010 psy-6330)
			:add-list '(psy-5330))
		(make-op :action 'take-psy-5500
			:add-list '(psy-5500))
		(make-op :action 'take-psy-5600
			:add-list '(psy-5600))
		(make-op :action 'take-psy-5900
			:add-list '(psy-5900))
		(make-op :action 'take-psy-5910
			:add-list '(psy-5910))
		(make-op :action 'take-psy-5930
			:add-list '(psy-5930))
		(make-op :action 'take-pubh-3120
			:add-list '(pubh-3120))
		(make-op :action 'take-pubh-3310
			:add-list '(pubh-3310))
		(make-op :action 'take-pubh-3610
			:preconds '(biol-1610 chem-1210 math-1210 cee-3610)
			:add-list '(pubh-3610))
		(make-op :action 'take-pubh-3870
			:preconds '(cee-3610)
			:add-list '(pubh-3870))
		(make-op :action 'take-pubh-4000
			:add-list '(pubh-4000))
		(make-op :action 'take-pubh-4030
			:preconds '(biol-1610 biol-2060 biol-2420 biol-4030)
			:add-list '(pubh-4030))
		(make-op :action 'take-pubh-4040
			:preconds '(biol-4040)
			:add-list '(pubh-4040))
		(make-op :action 'take-pubh-4200
			:add-list '(pubh-4200))
		(make-op :action 'take-pubh-4300
			:add-list '(pubh-4300))
		(make-op :action 'take-pubh-4310
			:preconds '(pubh-3310)
			:add-list '(pubh-4310))
		(make-op :action 'take-pubh-4320
			:preconds '(pubh-3310)
			:add-list '(pubh-4320))
		(make-op :action 'take-pubh-4330
			:preconds '(pubh-3310 pubh-4310)
			:add-list '(pubh-4330))
		(make-op :action 'take-pubh-4380
			:preconds '(pubh-4300 pubh-4320 pubh-4330)
			:add-list '(pubh-4380))
		(make-op :action 'take-pubh-4410
			:add-list '(pubh-4410))
		(make-op :action 'take-pubh-4850
			:add-list '(pubh-4850))
		(make-op :action 'take-pubh-5330
			:preconds '(math-1210 pubh-4310)
			:add-list '(pubh-5330))
		(make-op :action 'take-pubh-5340
			:preconds '(pubh-4320 pubh-4330)
			:add-list '(pubh-5340))
		(make-op :action 'take-pubh-5400
			:preconds '(chem-1220 biol-1620 chem-2300 advs-5400 biol-5400)
			:add-list '(pubh-5400))
		(make-op :action 'take-pubh-5500
			:preconds '(pubh-4000 pubh-4380)
			:add-list '(pubh-5500))
		(make-op :action 'take-pubh-5670
			:preconds '(chem-1210)
			:add-list '(pubh-5670))
		(make-op :action 'take-pubh-5730
			:preconds '(chem-1210 chem-1215 cee-5730 pubh-6730)
			:add-list '(pubh-5730))
		(make-op :action 'take-pubh-5790
			:preconds '(chem-1220)
			:add-list '(pubh-5790))
		(make-op :action 'take-ram-1000
			:add-list '(ram-1000))
		(make-op :action 'take-ram-1100
			:add-list '(ram-1100))
		(make-op :action 'take-ram-1320
			:add-list '(ram-1320))
		(make-op :action 'take-ram-1330
			:add-list '(ram-1330))
		(make-op :action 'take-ram-1340
			:add-list '(ram-1340))
		(make-op :action 'take-ram-1500
			:add-list '(ram-1500))
		(make-op :action 'take-ram-1600
			:add-list '(ram-1600))
		(make-op :action 'take-ram-1700
			:add-list '(ram-1700))
		(make-op :action 'take-ram-1800
			:add-list '(ram-1800))
		(make-op :action 'take-ram-1888
			:add-list '(ram-1888))
		(make-op :action 'take-ram-1964
			:add-list '(ram-1964))
		(make-op :action 'take-ram-2000
			:add-list '(ram-2000))
		(make-op :action 'take-ram-2100
			:preconds '(ram-1100)
			:add-list '(ram-2100))
		(make-op :action 'take-ram-2250
			:add-list '(ram-2250))
		(make-op :action 'take-ram-2300
			:add-list '(ram-2300))
		(make-op :action 'take-ram-2380
			:add-list '(ram-2380))
		(make-op :action 'take-ram-2500
			:add-list '(ram-2500))
		(make-op :action 'take-ram-2977
			:add-list '(ram-2977))
		(make-op :action 'take-ram-2988
			:add-list '(ram-2988))
		(make-op :action 'take-ram-3000
			:add-list '(ram-3000))
		(make-op :action 'take-ram-3075
			:preconds '(ram-1000 ram-3000 ram-4000 ram-3200 ram-4500 ram-4600)
			:add-list '(ram-3075))
		(make-op :action 'take-ram-3100
			:preconds '(ram-1000 ram-2300)
			:add-list '(ram-3100))
		(make-op :action 'take-ram-3200
			:preconds '(ram-1000 ram-3000)
			:add-list '(ram-3200))
		(make-op :action 'take-ram-3400
			:add-list '(ram-3400))
		(make-op :action 'take-ram-3900
			:add-list '(ram-3900))
		(make-op :action 'take-ram-4000
			:add-list '(ram-4000))
		(make-op :action 'take-ram-4100
			:preconds '(engl-2010)
			:add-list '(ram-4100))
		(make-op :action 'take-ram-4250
			:preconds '(ram-1000)
			:add-list '(ram-4250))
		(make-op :action 'take-ram-4400
			:preconds '(ram-1000)
			:add-list '(ram-4400))
		(make-op :action 'take-ram-4500
			:preconds '(ram-1000)
			:add-list '(ram-4500))
		(make-op :action 'take-ram-4600
			:preconds '(ram-1000)
			:add-list '(ram-4600))
		(make-op :action 'take-ram-4700
			:preconds '(ram-1000 ram-3000 ram-4000 ram-3200 ram-3075 ram-3900 ram-4100 ram-3200)
			:add-list '(ram-4700))
		(make-op :action 'take-ram-4800
			:preconds '(ram-1000 engl-2010)
			:add-list '(ram-4800))
		(make-op :action 'take-ram-4900
			:preconds '(ram-1000 ram-2250)
			:add-list '(ram-4900))
		(make-op :action 'take-ram-4970
			:add-list '(ram-4970))
		(make-op :action 'take-ram-5900
			:add-list '(ram-5900))
		(make-op :action 'take-ram-5910
			:add-list '(ram-5910))
		(make-op :action 'take-reh-1010
			:preconds '(sped-1010)
			:add-list '(reh-1010))
		(make-op :action 'take-rels-1010
			:add-list '(rels-1010))
		(make-op :action 'take-rels-1070
			:add-list '(rels-1070))
		(make-op :action 'take-rels-1090
			:preconds '(anth-1090)
			:add-list '(rels-1090))
		(make-op :action 'take-rels-1600
			:add-list '(rels-1600))
		(make-op :action 'take-rels-1680
			:add-list '(rels-1680))
		(make-op :action 'take-rels-2050
			:add-list '(rels-2050))
		(make-op :action 'take-rels-3010
			:preconds '(hist-3010)
			:add-list '(rels-3010))
		(make-op :action 'take-rels-3020
			:preconds '(hist-3020)
			:add-list '(rels-3020))
		(make-op :action 'take-rels-3030
			:preconds '(hist-3030)
			:add-list '(rels-3030))
		(make-op :action 'take-rels-3050
			:add-list '(rels-3050))
		(make-op :action 'take-rels-3060
			:preconds '(hist-3060)
			:add-list '(rels-3060))
		(make-op :action 'take-rels-3080
			:preconds '(hist-3080)
			:add-list '(rels-3080))
		(make-op :action 'take-rels-3090
			:add-list '(rels-3090))
		(make-op :action 'take-rels-3120
			:add-list '(rels-3120))
		(make-op :action 'take-rels-3125
			:add-list '(rels-3125))
		(make-op :action 'take-rels-3150
			:preconds '(phil-1000 phil-3020 rels-3120)
			:add-list '(rels-3150))
		(make-op :action 'take-rels-3160
			:preconds '(anth-3160)
			:add-list '(rels-3160))
		(make-op :action 'take-rels-3165
			:preconds '(anth-3165)
			:add-list '(rels-3165))
		(make-op :action 'take-rels-3170
			:preconds '(clas-3170 hist-3170)
			:add-list '(rels-3170))
		(make-op :action 'take-rels-3190
			:add-list '(rels-3190))
		(make-op :action 'take-rels-3210
			:preconds '(arth-3210 clas-3210 engl-3210 hist-3210)
			:add-list '(rels-3210))
		(make-op :action 'take-rels-3220
			:preconds '(hist-3220)
			:add-list '(rels-3220))
		(make-op :action 'take-rels-3270
			:preconds '(hist-3270)
			:add-list '(rels-3270))
		(make-op :action 'take-rels-3290
			:preconds '(clas-3290 hist-3290)
			:add-list '(rels-3290))
		(make-op :action 'take-rels-3410
			:preconds '(hist-3410)
			:add-list '(rels-3410))
		(make-op :action 'take-rels-3420
			:add-list '(rels-3420))
		(make-op :action 'take-rels-3430
			:preconds '(hist-3430)
			:add-list '(rels-3430))
		(make-op :action 'take-rels-3456
			:preconds '(hist-3456)
			:add-list '(rels-3456))
		(make-op :action 'take-rels-3470
			:preconds '(hist-3470)
			:add-list '(rels-3470))
		(make-op :action 'take-rels-3482
			:preconds '(hist-3482)
			:add-list '(rels-3482))
		(make-op :action 'take-rels-3510
			:add-list '(rels-3510))
		(make-op :action 'take-rels-3710
			:preconds '(anth-3710 engl-3710 hist-3710)
			:add-list '(rels-3710))
		(make-op :action 'take-rels-3820
			:preconds '(rels-1010 rels-3020 rels-3420)
			:add-list '(rels-3820))
		(make-op :action 'take-rels-3830
			:preconds '(engl-3830 hist-3830 kin-3830)
			:add-list '(rels-3830))
		(make-op :action 'take-rels-3990
			:preconds '(rels-1010)
			:add-list '(rels-3990))
		(make-op :action 'take-rels-4015
			:preconds '(hist-4015)
			:add-list '(rels-4015))
		(make-op :action 'take-rels-4230
			:preconds '(hist-4230)
			:add-list '(rels-4230))
		(make-op :action 'take-rels-4350
			:preconds '(clas-3210 hist-4350)
			:add-list '(rels-4350))
		(make-op :action 'take-rels-4520
			:add-list '(rels-4520))
		(make-op :action 'take-rels-4555
			:preconds '(hist-4555)
			:add-list '(rels-4555))
		(make-op :action 'take-rels-4560
			:add-list '(rels-4560))
		(make-op :action 'take-rels-4565
			:preconds '(hist-4565)
			:add-list '(rels-4565))
		(make-op :action 'take-rels-4566
			:preconds '(hist-4566)
			:add-list '(rels-4566))
		(make-op :action 'take-rels-4567
			:preconds '(hist-4567)
			:add-list '(rels-4567))
		(make-op :action 'take-rels-4730
			:preconds '(hist-4730)
			:add-list '(rels-4730))
		(make-op :action 'take-rels-4790
			:preconds '(hist-4790)
			:add-list '(rels-4790))
		(make-op :action 'take-rels-4825
			:preconds '(hist-4825)
			:add-list '(rels-4825))
		(make-op :action 'take-rels-4910
			:add-list '(rels-4910))
		(make-op :action 'take-rels-4930
			:add-list '(rels-4930))
		(make-op :action 'take-rels-4990
			:preconds '(rels-3990)
			:add-list '(rels-4990))
		(make-op :action 'take-rels-5195
			:preconds '(anth-1090 rels-1090 anth-3160 rels-1010 anth-5195)
			:add-list '(rels-5195))
		(make-op :action 'take-rels-5740
			:preconds '(arth-5740)
			:add-list '(rels-5740))
		(make-op :action 'take-russ-1010
			:add-list '(russ-1010))
		(make-op :action 'take-russ-1020
			:preconds '(russ-1010)
			:add-list '(russ-1020))
		(make-op :action 'take-russ-1030
			:add-list '(russ-1030))
		(make-op :action 'take-russ-1050
			:preconds '(russ-1010)
			:add-list '(russ-1050))
		(make-op :action 'take-russ-2010
			:preconds '(russ-1020)
			:add-list '(russ-2010))
		(make-op :action 'take-russ-2020
			:preconds '(russ-2010)
			:add-list '(russ-2020))
		(make-op :action 'take-russ-2700
			:add-list '(russ-2700))
		(make-op :action 'take-russ-2880
			:add-list '(russ-2880))
		(make-op :action 'take-russ-2988
			:add-list '(russ-2988))
		(make-op :action 'take-russ-3040
			:preconds '(russ-2020)
			:add-list '(russ-3040))
		(make-op :action 'take-russ-3050
			:preconds '(russ-2020)
			:add-list '(russ-3050))
		(make-op :action 'take-russ-3300
			:preconds '(russ-2020)
			:add-list '(russ-3300))
		(make-op :action 'take-russ-3510
			:preconds '(russ-2020)
			:add-list '(russ-3510))
		(make-op :action 'take-russ-3540
			:preconds '(russ-2020)
			:add-list '(russ-3540))
		(make-op :action 'take-russ-3600
			:preconds '(russ-2020)
			:add-list '(russ-3600))
		(make-op :action 'take-russ-4880
			:add-list '(russ-4880))
		(make-op :action 'take-russ-4920
			:add-list '(russ-4920))
		(make-op :action 'take-sci-1050
			:add-list '(sci-1050))
		(make-op :action 'take-sci-2050
			:add-list '(sci-2050))
		(make-op :action 'take-sci-4250
			:add-list '(sci-4250))
		(make-op :action 'take-sced-1000
			:add-list '(sced-1000))
		(make-op :action 'take-sced-3210
			:add-list '(sced-3210))
		(make-op :action 'take-sced-3300
			:add-list '(sced-3300))
		(make-op :action 'take-sced-3400
			:add-list '(sced-3400))
		(make-op :action 'take-sced-3500
			:preconds '(sced-3300 sced-4300)
			:add-list '(sced-3500))
		(make-op :action 'take-sced-3550
			:preconds '(sced-3300 sced-4300)
			:add-list '(sced-3550))
		(make-op :action 'take-sced-4300
			:add-list '(sced-4300))
		(make-op :action 'take-sced-4900
			:add-list '(sced-4900))
		(make-op :action 'take-sced-5100
			:add-list '(sced-5100))
		(make-op :action 'take-sced-5200
			:add-list '(sced-5200))
		(make-op :action 'take-sced-5210
			:add-list '(sced-5210))
		(make-op :action 'take-sced-5240
			:add-list '(sced-5240))
		(make-op :action 'take-sced-5400
			:add-list '(sced-5400))
		(make-op :action 'take-sced-5500
			:add-list '(sced-5500))
		(make-op :action 'take-sced-5630
			:add-list '(sced-5630))
		(make-op :action 'take-sced-5700
			:add-list '(sced-5700))
		(make-op :action 'take-sced-5810
			:add-list '(sced-5810))
		(make-op :action 'take-sced-5820
			:add-list '(sced-5820))
		(make-op :action 'take-sced-5821
			:preconds '(sced-5820)
			:add-list '(sced-5821))
		(make-op :action 'take-sced-5900
			:add-list '(sced-5900))
		(make-op :action 'take-slsc-1010
			:add-list '(slsc-1010))
		(make-op :action 'take-slsc-1030
			:add-list '(slsc-1030))
		(make-op :action 'take-slsc-1110
			:add-list '(slsc-1110))
		(make-op :action 'take-slsc-1120
			:add-list '(slsc-1120))
		(make-op :action 'take-slsc-1130
			:add-list '(slsc-1130))
		(make-op :action 'take-slsc-1140
			:add-list '(slsc-1140))
		(make-op :action 'take-slsc-1150
			:add-list '(slsc-1150))
		(make-op :action 'take-slsc-1300
			:add-list '(slsc-1300))
		(make-op :action 'take-slsc-1800
			:add-list '(slsc-1800))
		(make-op :action 'take-sw-1010
			:add-list '(sw-1010))
		(make-op :action 'take-sw-2100
			:add-list '(sw-2100))
		(make-op :action 'take-sw-2250
			:add-list '(sw-2250))
		(make-op :action 'take-sw-2400
			:add-list '(sw-2400))
		(make-op :action 'take-sw-3050
			:preconds '(sw-1010 sw-2100 sw-2400)
			:add-list '(sw-3050))
		(make-op :action 'take-sw-3250
			:preconds '(sw-1010 sw-2100 sw-2400)
			:add-list '(sw-3250))
		(make-op :action 'take-sw-3350
			:add-list '(sw-3350))
		(make-op :action 'take-sw-3360
			:add-list '(sw-3360))
		(make-op :action 'take-sw-3400
			:preconds '(sw-1010)
			:add-list '(sw-3400))
		(make-op :action 'take-sw-3450
			:add-list '(sw-3450))
		(make-op :action 'take-sw-3650
			:add-list '(sw-3650))
		(make-op :action 'take-sw-3750
			:add-list '(sw-3750))
		(make-op :action 'take-sw-3850
			:add-list '(sw-3850))
		(make-op :action 'take-sw-4010
			:preconds '(span-2020 sw-1010 span-4010 sw-6010)
			:add-list '(sw-4010))
		(make-op :action 'take-sw-4100
			:preconds '(sw-1010 sw-2100 sw-2400)
			:add-list '(sw-4100))
		(make-op :action 'take-sw-4150
			:preconds '(sw-3050)
			:add-list '(sw-4150))
		(make-op :action 'take-sw-4160
			:preconds '(sw-3050)
			:add-list '(sw-4160))
		(make-op :action 'take-sw-4525
			:preconds '(sw-6525)
			:add-list '(sw-4525))
		(make-op :action 'take-sw-4540
			:preconds '(sw-6540)
			:add-list '(sw-4540))
		(make-op :action 'take-sw-4580
			:preconds '(sw-6580)
			:add-list '(sw-4580))
		(make-op :action 'take-sw-4870
			:add-list '(sw-4870))
		(make-op :action 'take-sw-4900
			:preconds '(sw-1010 sw-2100 sw-2400)
			:add-list '(sw-4900))
		(make-op :action 'take-sw-4920
			:preconds '(sw-6920)
			:add-list '(sw-4920))
		(make-op :action 'take-sw-4925
			:add-list '(sw-4925))
		(make-op :action 'take-sw-4950
			:add-list '(sw-4950))
		(make-op :action 'take-sw-5350
			:preconds '(sw-1010 sw-2100 sw-2400 sw-4150 sw-4160)
			:add-list '(sw-5350))
		(make-op :action 'take-sw-5870
			:preconds '(sw-4870)
			:add-list '(sw-5870))
		(make-op :action 'take-sw-5901
			:add-list '(sw-5901))
		(make-op :action 'take-sw-5902
			:add-list '(sw-5902))
		(make-op :action 'take-sw-5903
			:add-list '(sw-5903))
		(make-op :action 'take-sw-5905
			:add-list '(sw-5905))
		(make-op :action 'take-sw-5906
			:preconds '(sw-5905)
			:add-list '(sw-5906))
		(make-op :action 'take-sw-5907
			:preconds '(sw-5905 sw-5906)
			:add-list '(sw-5907))
		(make-op :action 'take-cj-4200
			:add-list '(cj-4200))
		(make-op :action 'take-cj-4440
			:add-list '(cj-4440))
		(make-op :action 'take-cj-4770
			:preconds '(cj-4200)
			:add-list '(cj-4770))
		(make-op :action 'take-soc-1010
			:add-list '(soc-1010))
		(make-op :action 'take-soc-1020
			:add-list '(soc-1020))
		(make-op :action 'take-soc-2120
			:add-list '(soc-2120))
		(make-op :action 'take-soc-2370
			:add-list '(soc-2370))
		(make-op :action 'take-soc-2500
			:add-list '(soc-2500))
		(make-op :action 'take-soc-2600
			:add-list '(soc-2600))
		(make-op :action 'take-soc-2630
			:add-list '(soc-2630))
		(make-op :action 'take-soc-2650
			:add-list '(soc-2650))
		(make-op :action 'take-soc-2800
			:add-list '(soc-2800))
		(make-op :action 'take-soc-2988
			:add-list '(soc-2988))
		(make-op :action 'take-soc-2999
			:add-list '(soc-2999))
		(make-op :action 'take-soc-3010
			:add-list '(soc-3010))
		(make-op :action 'take-soc-3110
			:add-list '(soc-3110))
		(make-op :action 'take-soc-3120
			:add-list '(soc-3120))
		(make-op :action 'take-soc-3200
			:add-list '(soc-3200))
		(make-op :action 'take-soc-3260
			:preconds '(pols-3260)
			:add-list '(soc-3260))
		(make-op :action 'take-soc-3300
			:add-list '(soc-3300))
		(make-op :action 'take-soc-3310
			:add-list '(soc-3310))
		(make-op :action 'take-soc-3320
			:add-list '(soc-3320))
		(make-op :action 'take-soc-3330
			:add-list '(soc-3330))
		(make-op :action 'take-soc-3340
			:add-list '(soc-3340))
		(make-op :action 'take-soc-3400
			:preconds '(cj-1010 cj-1330)
			:add-list '(soc-3400))
		(make-op :action 'take-soc-3410
			:add-list '(soc-3410))
		(make-op :action 'take-soc-3420
			:add-list '(soc-3420))
		(make-op :action 'take-soc-3430
			:add-list '(soc-3430))
		(make-op :action 'take-soc-3500
			:add-list '(soc-3500))
		(make-op :action 'take-soc-3520
			:add-list '(soc-3520))
		(make-op :action 'take-soc-3600
			:add-list '(soc-3600))
		(make-op :action 'take-soc-3610
			:add-list '(soc-3610))
		(make-op :action 'take-soc-3650
			:add-list '(soc-3650))
		(make-op :action 'take-soc-3750
			:add-list '(soc-3750))
		(make-op :action 'take-soc-4010
			:add-list '(soc-4010))
		(make-op :action 'take-soc-4230
			:preconds '(soc-6230)
			:add-list '(soc-4230))
		(make-op :action 'take-soc-4350
			:preconds '(soc-7350)
			:add-list '(soc-4350))
		(make-op :action 'take-soc-4410
			:preconds '(soc-3420)
			:add-list '(soc-4410))
		(make-op :action 'take-soc-4420
			:add-list '(soc-4420))
		(make-op :action 'take-soc-4600
			:preconds '(soc-1010 soc-3010 soc-3110 soc-3120 soc-4010)
			:add-list '(soc-4600))
		(make-op :action 'take-soc-4620
			:add-list '(soc-4620))
		(make-op :action 'take-soc-4640
			:add-list '(soc-4640))
		(make-op :action 'take-soc-4720
			:add-list '(soc-4720))
		(make-op :action 'take-soc-4800
			:add-list '(soc-4800))
		(make-op :action 'take-soc-4900
			:add-list '(soc-4900))
		(make-op :action 'take-soc-5120
			:add-list '(soc-5120))
		(make-op :action 'take-soc-5130
			:preconds '(anth-5130 soc-6130)
			:add-list '(soc-5130))
		(make-op :action 'take-soc-5270
			:preconds '(soc-6270)
			:add-list '(soc-5270))
		(make-op :action 'take-soc-5420
			:add-list '(soc-5420))
		(make-op :action 'take-soc-5460
			:add-list '(soc-5460))
		(make-op :action 'take-soc-5620
			:add-list '(soc-5620))
		(make-op :action 'take-soc-5640
			:add-list '(soc-5640))
		(make-op :action 'take-span-1010
			:add-list '(span-1010))
		(make-op :action 'take-span-1020
			:preconds '(span-1010)
			:add-list '(span-1020))
		(make-op :action 'take-span-1050
			:preconds '(span-1010 span-1020)
			:add-list '(span-1050))
		(make-op :action 'take-span-1800
			:add-list '(span-1800))
		(make-op :action 'take-span-2010
			:preconds '(span-1020)
			:add-list '(span-2010))
		(make-op :action 'take-span-2020
			:preconds '(span-2010)
			:add-list '(span-2020))
		(make-op :action 'take-span-2800
			:preconds '(span-1020)
			:add-list '(span-2800))
		(make-op :action 'take-span-2988
			:add-list '(span-2988))
		(make-op :action 'take-span-3010
			:add-list '(span-3010))
		(make-op :action 'take-span-3040
			:preconds '(span-2020)
			:add-list '(span-3040))
		(make-op :action 'take-span-3041
			:add-list '(span-3041))
		(make-op :action 'take-span-3060
			:preconds '(span-2020)
			:add-list '(span-3060))
		(make-op :action 'take-span-3061
			:preconds '(span-3040 span-3060)
			:add-list '(span-3061))
		(make-op :action 'take-span-3081
			:preconds '(span-3061)
			:add-list '(span-3081))
		(make-op :action 'take-span-3082
			:preconds '(span-3061)
			:add-list '(span-3082))
		(make-op :action 'take-span-3100
			:preconds '(span-3040)
			:add-list '(span-3100))
		(make-op :action 'take-span-3116
			:add-list '(span-3116))
		(make-op :action 'take-span-3117
			:add-list '(span-3117))
		(make-op :action 'take-span-3118
			:add-list '(span-3118))
		(make-op :action 'take-span-3300
			:preconds '(span-2020)
			:add-list '(span-3300))
		(make-op :action 'take-span-3400
			:preconds '(span-3300)
			:add-list '(span-3400))
		(make-op :action 'take-span-3510
			:preconds '(span-2020)
			:add-list '(span-3510))
		(make-op :action 'take-span-3520
			:add-list '(span-3520))
		(make-op :action 'take-span-3550
			:preconds '(span-2020)
			:add-list '(span-3550))
		(make-op :action 'take-span-3560
			:preconds '(span-2020)
			:add-list '(span-3560))
		(make-op :action 'take-span-3570
			:preconds '(span-2020)
			:add-list '(span-3570))
		(make-op :action 'take-span-3600
			:add-list '(span-3600))
		(make-op :action 'take-span-3610
			:preconds '(span-3300)
			:add-list '(span-3610))
		(make-op :action 'take-span-3620
			:preconds '(span-3300)
			:add-list '(span-3620))
		(make-op :action 'take-span-3630
			:preconds '(span-3300)
			:add-list '(span-3630))
		(make-op :action 'take-span-3640
			:preconds '(span-3300)
			:add-list '(span-3640))
		(make-op :action 'take-span-3650
			:preconds '(span-3040)
			:add-list '(span-3650))
		(make-op :action 'take-span-3660
			:preconds '(span-3300)
			:add-list '(span-3660))
		(make-op :action 'take-span-3800
			:preconds '(span-2020)
			:add-list '(span-3800))
		(make-op :action 'take-span-4010
			:preconds '(span-2020 sw-1010 sw-4010)
			:add-list '(span-4010))
		(make-op :action 'take-span-4071
			:preconds '(span-3061 span-3081 span-3082)
			:add-list '(span-4071))
		(make-op :action 'take-span-4072
			:preconds '(span-4071)
			:add-list '(span-4072))
		(make-op :action 'take-span-4081
			:preconds '(span-4071 span-4072)
			:add-list '(span-4081))
		(make-op :action 'take-span-4082
			:preconds '(span-4072)
			:add-list '(span-4082))
		(make-op :action 'take-span-4200
			:preconds '(span-3040)
			:add-list '(span-4200))
		(make-op :action 'take-span-4800
			:preconds '(span-2020)
			:add-list '(span-4800))
		(make-op :action 'take-span-4880
			:add-list '(span-4880))
		(make-op :action 'take-span-4900
			:preconds '(span-3600 span-3610 span-3620 span-3630 span-3640 span-3650 span-3660)
			:add-list '(span-4900))
		(make-op :action 'take-span-4910
			:preconds '(span-3600 span-3610 span-3620 span-3630 span-3640 span-3650 span-3660)
			:add-list '(span-4910))
		(make-op :action 'take-span-4920
			:add-list '(span-4920))
		(make-op :action 'take-span-4930
			:preconds '(span-3600 span-3610 span-3620 span-3630 span-3640)
			:add-list '(span-4930))
		(make-op :action 'take-span-4940
			:preconds '(span-3600 span-3610 span-3620 span-3630 span-3640 span-3650 span-3660)
			:add-list '(span-4940))
		(make-op :action 'take-span-4990
			:add-list '(span-4990))
		(make-op :action 'take-span-5060
			:preconds '(span-3040 span-3060)
			:add-list '(span-5060))
		(make-op :action 'take-sped-1000
			:add-list '(sped-1000))
		(make-op :action 'take-sped-1010
			:preconds '(reh-1010)
			:add-list '(sped-1010))
		(make-op :action 'take-sped-2790
			:add-list '(sped-2790))
		(make-op :action 'take-sped-3104
			:add-list '(sped-3104))
		(make-op :action 'take-sped-4000
			:preconds '(comd-3200)
			:add-list '(sped-4000))
		(make-op :action 'take-sped-4106
			:preconds '(comd-6660 sped-6106)
			:add-list '(sped-4106))
		(make-op :action 'take-sped-4107
			:preconds '(sped-4106 comd-6660 comd-6840 sped-6107)
			:add-list '(sped-4107))
		(make-op :action 'take-sped-4108
			:preconds '(sped-4106 comd-6660 sped-4107 comd-6840)
			:add-list '(sped-4108))
		(make-op :action 'take-sped-4109
			:preconds '(sped-4106 comd-6660 sped-4107 comd-6840 comd-6890 sped-6109)
			:add-list '(sped-4109))
		(make-op :action 'take-sped-4300
			:add-list '(sped-4300))
		(make-op :action 'take-sped-4790
			:add-list '(sped-4790))
		(make-op :action 'take-sped-4910
			:add-list '(sped-4910))
		(make-op :action 'take-sped-4970
			:add-list '(sped-4970))
		(make-op :action 'take-sped-5010
			:preconds '(sped-4000 sped-5030 sped-5040)
			:add-list '(sped-5010))
		(make-op :action 'take-sped-5011
			:add-list '(sped-5011))
		(make-op :action 'take-sped-5012
			:add-list '(sped-5012))
		(make-op :action 'take-sped-5013
			:preconds '(sped-5011 sped-5012 sped-5014)
			:add-list '(sped-5013))
		(make-op :action 'take-sped-5014
			:preconds '(sped-5011 sped-5012)
			:add-list '(sped-5014))
		(make-op :action 'take-sped-5015
			:preconds '(sped-5011 sped-5012 sped-5014)
			:add-list '(sped-5015))
		(make-op :action 'take-sped-5016
			:preconds '(sped-5011 sped-5012 sped-5014 sped-5015)
			:add-list '(sped-5016))
		(make-op :action 'take-sped-5017
			:preconds '(sped-5013 sped-5016 sped-5018)
			:add-list '(sped-5017))
		(make-op :action 'take-sped-5018
			:preconds '(sped-5011 sped-5012)
			:add-list '(sped-5018))
		(make-op :action 'take-sped-5030
			:add-list '(sped-5030))
		(make-op :action 'take-sped-5040
			:add-list '(sped-5040))
		(make-op :action 'take-sped-5050
			:preconds '(sped-5010 sped-5070)
			:add-list '(sped-5050))
		(make-op :action 'take-sped-5060
			:preconds '(sped-5010 sped-5070)
			:add-list '(sped-5060))
		(make-op :action 'take-sped-5070
			:preconds '(sped-4000 sped-5040)
			:add-list '(sped-5070))
		(make-op :action 'take-sped-5080
			:preconds '(sped-5010 sped-5070)
			:add-list '(sped-5080))
		(make-op :action 'take-sped-5105
			:add-list '(sped-5105))
		(make-op :action 'take-sped-5110
			:preconds '(sped-5010 sped-5070 sped-5105 sped-5140 sped-5170 sped-5180)
			:add-list '(sped-5110))
		(make-op :action 'take-sped-5120
			:preconds '(sped-5105)
			:add-list '(sped-5120))
		(make-op :action 'take-sped-5122
			:preconds '(sped-5105)
			:add-list '(sped-5122))
		(make-op :action 'take-sped-5130
			:preconds '(sped-5105)
			:add-list '(sped-5130))
		(make-op :action 'take-sped-5135
			:preconds '(sped-5010 sped-5070 sped-5140 sped-5170 sped-5180)
			:add-list '(sped-5135))
		(make-op :action 'take-sped-5140
			:preconds '(sped-4000 sped-5040 sped-5120 sped-5130)
			:add-list '(sped-5140))
		(make-op :action 'take-sped-5150
			:preconds '(sped-5010 sped-5070 sped-5140 sped-5170 sped-5180)
			:add-list '(sped-5150))
		(make-op :action 'take-sped-5160
			:preconds '(sped-5010 sped-5070 sped-5140 sped-5170 sped-5180)
			:add-list '(sped-5160))
		(make-op :action 'take-sped-5170
			:preconds '(sped-4000 sped-5040 sped-5120 sped-5130)
			:add-list '(sped-5170))
		(make-op :action 'take-sped-5175
			:preconds '(sped-5010 sped-5070 sped-5140 sped-5170 sped-5180)
			:add-list '(sped-5175))
		(make-op :action 'take-sped-5180
			:preconds '(sped-5130 sped-5170)
			:add-list '(sped-5180))
		(make-op :action 'take-sped-5190
			:preconds '(sped-5010 sped-5070 sped-5140 sped-5170 sped-5180)
			:add-list '(sped-5190))
		(make-op :action 'take-sped-5200
			:add-list '(sped-5200))
		(make-op :action 'take-sped-5210
			:add-list '(sped-5210))
		(make-op :action 'take-sped-5220
			:preconds '(sped-5200 sped-5210)
			:add-list '(sped-5220))
		(make-op :action 'take-sped-5230
			:add-list '(sped-5230))
		(make-op :action 'take-sped-5310
			:preconds '(eled-3100 sped-4000 sped-5040)
			:add-list '(sped-5310))
		(make-op :action 'take-sped-5311
			:add-list '(sped-5311))
		(make-op :action 'take-sped-5312
			:preconds '(sped-5011 sped-5012 sped-5311 sped-5415)
			:add-list '(sped-5312))
		(make-op :action 'take-sped-5320
			:preconds '(sped-5010 sped-5070 sped-5310 sped-5340 sped-5410)
			:add-list '(sped-5320))
		(make-op :action 'take-sped-5330
			:preconds '(eled-3100 sped-4000 sped-5040 teal-2660)
			:add-list '(sped-5330))
		(make-op :action 'take-sped-5340
			:preconds '(eled-3100 sped-4000 sped-5040 teal-2660)
			:add-list '(sped-5340))
		(make-op :action 'take-sped-5350
			:add-list '(sped-5350))
		(make-op :action 'take-sped-5360
			:add-list '(sped-5360))
		(make-op :action 'take-sped-5410
			:preconds '(eled-3100 sped-4000 sped-5040)
			:add-list '(sped-5410))
		(make-op :action 'take-sped-5415
			:add-list '(sped-5415))
		(make-op :action 'take-sped-5420
			:preconds '(sped-5010 sped-5070 sped-5310 sped-5340 sped-5410)
			:add-list '(sped-5420))
		(make-op :action 'take-sped-5425
			:preconds '(sped-5011 sped-5012 sped-5311 sped-5415)
			:add-list '(sped-5425))
		(make-op :action 'take-sped-5430
			:add-list '(sped-5430))
		(make-op :action 'take-sped-5500
			:add-list '(sped-5500))
		(make-op :action 'take-sped-5510
			:preconds '(eled-3100 sped-4000 sped-5040)
			:add-list '(sped-5510))
		(make-op :action 'take-sped-5511
			:add-list '(sped-5511))
		(make-op :action 'take-sped-5512
			:preconds '(sped-5011 sped-5012 sped-5511 sped-5615)
			:add-list '(sped-5512))
		(make-op :action 'take-sped-5520
			:preconds '(sped-5010 sped-5070 sped-5510 sped-5600)
			:add-list '(sped-5520))
		(make-op :action 'take-sped-5530
			:preconds '(sped-5010 sped-5070)
			:add-list '(sped-5530))
		(make-op :action 'take-sped-5540
			:preconds '(eled-3100 sped-4000 sped-5040 teal-2660)
			:add-list '(sped-5540))
		(make-op :action 'take-sped-5600
			:preconds '(eled-3100 sped-4000 sped-5040)
			:add-list '(sped-5600))
		(make-op :action 'take-sped-5610
			:preconds '(sped-5010 sped-5070 sped-5510 sped-5600)
			:add-list '(sped-5610))
		(make-op :action 'take-sped-5615
			:add-list '(sped-5615))
		(make-op :action 'take-sped-5625
			:preconds '(sped-5011 sped-5012 sped-5511 sped-5615)
			:add-list '(sped-5625))
		(make-op :action 'take-sped-5630
			:add-list '(sped-5630))
		(make-op :action 'take-sped-5640
			:preconds '(sped-5630)
			:add-list '(sped-5640))
		(make-op :action 'take-sped-5710
			:preconds '(hdfs-2600 hdfs-2630 hdfs-3500 sped-4000 sped-5040)
			:add-list '(sped-5710))
		(make-op :action 'take-sped-5711
			:add-list '(sped-5711))
		(make-op :action 'take-sped-5712
			:preconds '(sped-5011 sped-5012 sped-5711)
			:add-list '(sped-5712))
		(make-op :action 'take-sped-5713
			:preconds '(sped-5014 sped-5712 sped-5714)
			:add-list '(sped-5713))
		(make-op :action 'take-sped-5714
			:preconds '(sped-5011 sped-5012 sped-5711)
			:add-list '(sped-5714))
		(make-op :action 'take-sped-5790
			:add-list '(sped-5790))
		(make-op :action 'take-sped-5810
			:preconds '(hdfs-2600 hdfs-2630 hdfs-3500 sped-4000 sped-5040)
			:add-list '(sped-5810))
		(make-op :action 'take-sped-5820
			:preconds '(sped-5010 sped-5070 sped-5310 sped-5710 sped-5810 sped-5840)
			:add-list '(sped-5820))
		(make-op :action 'take-sped-5840
			:preconds '(hdfs-2600 hdfs-2630 sped-4000 sped-5040)
			:add-list '(sped-5840))
		(make-op :action 'take-sped-5880
			:preconds '(hdfs-2600 hdfs-2630 sped-4000 sped-5040)
			:add-list '(sped-5880))
		(make-op :action 'take-sped-5900
			:add-list '(sped-5900))
		(make-op :action 'take-sped-5910
			:add-list '(sped-5910))
		(make-op :action 'take-sped-5920
			:preconds '(sped-5710 sped-5810)
			:add-list '(sped-5920))
		(make-op :action 'take-stat-1040
			:preconds '(math-1050 math-1100)
			:add-list '(stat-1040))
		(make-op :action 'take-stat-1045
			:preconds '(math-1050)
			:add-list '(stat-1045))
		(make-op :action 'take-stat-1080
			:preconds '(math-1050 math-1100)
			:add-list '(stat-1080))
		(make-op :action 'take-stat-2000
			:preconds '(math-1050 math-1100)
			:add-list '(stat-2000))
		(make-op :action 'take-stat-2250
			:add-list '(stat-2250))
		(make-op :action 'take-stat-2300
			:preconds '(math-1050 math-1100)
			:add-list '(stat-2300))
		(make-op :action 'take-stat-2950
			:add-list '(stat-2950))
		(make-op :action 'take-stat-3000
			:preconds '(math-1100 math-1210)
			:add-list '(stat-3000))
		(make-op :action 'take-stat-3080
			:preconds '(math-1100 math-1210)
			:add-list '(stat-3080))
		(make-op :action 'take-stat-4010
			:preconds '(stat-3000)
			:add-list '(stat-4010))
		(make-op :action 'take-stat-4250
			:add-list '(stat-4250))
		(make-op :action 'take-stat-4500
			:preconds '(math-3110 math-4200 math-4310 stat-2000 stat-3000 math-4500)
			:add-list '(stat-4500))
		(make-op :action 'take-stat-4950
			:add-list '(stat-4950))
		(make-op :action 'take-stat-5050
			:preconds '(stat-2000)
			:add-list '(stat-5050))
		(make-op :action 'take-stat-5080
			:preconds '(stat-3000 stat-5100 stat-5050 stat-5550 stat-6080)
			:add-list '(stat-5080))
		(make-op :action 'take-stat-5100
			:preconds '(stat-2000 stat-3000)
			:add-list '(stat-5100))
		(make-op :action 'take-stat-5120
			:preconds '(stat-5100)
			:add-list '(stat-5120))
		(make-op :action 'take-stat-5150
			:preconds '(stat-5650 stat-6150)
			:add-list '(stat-5150))
		(make-op :action 'take-stat-5170
			:preconds '(stat-5100 stat-5050 stat-6170)
			:add-list '(stat-5170))
		(make-op :action 'take-stat-5200
			:preconds '(stat-2000 stat-2300 stat-3000)
			:add-list '(stat-5200))
		(make-op :action 'take-stat-5410
			:preconds '(stat-3000 stat-5100 stat-5050 stat-5550 stat-5560 stat-6560 stat-6410)
			:add-list '(stat-5410))
		(make-op :action 'take-stat-5500
			:preconds '(stat-5100 stat-5200 stat-6500)
			:add-list '(stat-5500))
		(make-op :action 'take-stat-5550
			:preconds '(stat-5050)
			:add-list '(stat-5550))
		(make-op :action 'take-stat-5555
			:preconds '(stat-5050 stat-6555)
			:add-list '(stat-5555))
		(make-op :action 'take-stat-5560
			:preconds '(stat-5050 stat-5550 stat-6560)
			:add-list '(stat-5560))
		(make-op :action 'take-stat-5570
			:preconds '(stat-5100 stat-5200 stat-6570)
			:add-list '(stat-5570))
		(make-op :action 'take-stat-5645
			:preconds '(math-1210 stat-3000 math-5710 math-1220 math-2270 math-5645 stat-6645)
			:add-list '(stat-5645))
		(make-op :action 'take-stat-5650
			:preconds '(stat-5100)
			:add-list '(stat-5650))
		(make-op :action 'take-stat-5685
			:preconds '(stat-6685)
			:add-list '(stat-5685))
		(make-op :action 'take-stat-5810
			:add-list '(stat-5810))
		(make-op :action 'take-stat-5820
			:add-list '(stat-5820))
		(make-op :action 'take-stat-5940
			:add-list '(stat-5940))
		(make-op :action 'take-stat-5950
			:add-list '(stat-5950))
		(make-op :action 'take-stat-5970
			:add-list '(stat-5970))
		(make-op :action 'take-surg-1030
			:add-list '(surg-1030))
		(make-op :action 'take-surg-1035
			:add-list '(surg-1035))
		(make-op :action 'take-surg-1040
			:preconds '(surg-1030)
			:add-list '(surg-1040))
		(make-op :action 'take-surg-1045
			:preconds '(surg-1035 surg-1035)
			:add-list '(surg-1045))
		(make-op :action 'take-surg-1050
			:add-list '(surg-1050))
		(make-op :action 'take-surg-1055
			:add-list '(surg-1055))
		(make-op :action 'take-surg-1060
			:add-list '(surg-1060))
		(make-op :action 'take-surg-1065
			:add-list '(surg-1065))
		(make-op :action 'take-surg-1600
			:add-list '(surg-1600))
		(make-op :action 'take-surg-1700
			:preconds '(surg-1600 surg-1600)
			:add-list '(surg-1700))
		(make-op :action 'take-surg-1800
			:add-list '(surg-1800))
		(make-op :action 'take-surg-1900
			:add-list '(surg-1900))
		(make-op :action 'take-teal-1001
			:add-list '(teal-1001))
		(make-op :action 'take-teal-1002
			:add-list '(teal-1002))
		(make-op :action 'take-teal-1003
			:add-list '(teal-1003))
		(make-op :action 'take-teal-1004
			:add-list '(teal-1004))
		(make-op :action 'take-teal-1005
			:add-list '(teal-1005))
		(make-op :action 'take-teal-1010
			:add-list '(teal-1010))
		(make-op :action 'take-teal-2660
			:preconds '(hdfs-1500 eled-1010 teal-1010)
			:add-list '(teal-2660))
		(make-op :action 'take-teal-3780
			:preconds '(hist-3780)
			:add-list '(teal-3780))
		(make-op :action 'take-teal-4530
			:preconds '(teal-6530)
			:add-list '(teal-4530))
		(make-op :action 'take-teal-4600
			:preconds '(teal-6600)
			:add-list '(teal-4600))
		(make-op :action 'take-teal-4610
			:preconds '(teal-6610)
			:add-list '(teal-4610))
		(make-op :action 'take-teal-4620
			:preconds '(teal-6620)
			:add-list '(teal-4620))
		(make-op :action 'take-teal-4630
			:preconds '(eled-4062 eled-4063 math-1210 teal-6630)
			:add-list '(teal-4630))
		(make-op :action 'take-teal-4730
			:add-list '(teal-4730))
		(make-op :action 'take-teal-4745
			:add-list '(teal-4745))
		(make-op :action 'take-teal-4755
			:preconds '(teal-6755)
			:add-list '(teal-4755))
		(make-op :action 'take-teal-4760
			:add-list '(teal-4760))
		(make-op :action 'take-teal-4770
			:preconds '(teal-6770)
			:add-list '(teal-4770))
		(make-op :action 'take-teal-4780
			:preconds '(teal-6780)
			:add-list '(teal-4780))
		(make-op :action 'take-teal-4790
			:preconds '(teal-6790)
			:add-list '(teal-4790))
		(make-op :action 'take-teal-4900
			:add-list '(teal-4900))
		(make-op :action 'take-teal-5030
			:add-list '(teal-5030))
		(make-op :action 'take-teal-5080
			:preconds '(art-3700 musc-3260 thea-3320 thea-3330 thea-3340 thea-3380)
			:add-list '(teal-5080))
		(make-op :action 'take-teal-5205
			:add-list '(teal-5205))
		(make-op :action 'take-teal-5215
			:preconds '(eled-4020 eled-4056)
			:add-list '(teal-5215))
		(make-op :action 'take-teal-5230
			:add-list '(teal-5230))
		(make-op :action 'take-teal-5235
			:preconds '(eled-4030 eled-4040 eled-4042)
			:add-list '(teal-5235))
		(make-op :action 'take-teal-5320
			:add-list '(teal-5320))
		(make-op :action 'take-teal-5380
			:add-list '(teal-5380))
		(make-op :action 'take-teal-5390
			:preconds '(eled-4030 eled-4040 eled-4042)
			:add-list '(teal-5390))
		(make-op :action 'take-teal-5420
			:preconds '(teal-6420)
			:add-list '(teal-5420))
		(make-op :action 'take-teal-5440
			:add-list '(teal-5440))
		(make-op :action 'take-teal-5450
			:preconds '(teal-6450)
			:add-list '(teal-5450))
		(make-op :action 'take-teal-5460
			:preconds '(teal-6460)
			:add-list '(teal-5460))
		(make-op :action 'take-teal-5480
			:preconds '(teal-6480)
			:add-list '(teal-5480))
		(make-op :action 'take-teal-5500
			:add-list '(teal-5500))
		(make-op :action 'take-teal-5521
			:preconds '(eled-4062 eled-4056)
			:add-list '(teal-5521))
		(make-op :action 'take-teal-5523
			:preconds '(eled-4062 eled-4056)
			:add-list '(teal-5523))
		(make-op :action 'take-teal-5560
			:preconds '(educ-5560 educ-6560 teal-6560)
			:add-list '(teal-5560))
		(make-op :action 'take-teal-5630
			:add-list '(teal-5630))
		(make-op :action 'take-teal-5710
			:preconds '(eled-3000 eled-3001)
			:add-list '(teal-5710))
		(make-op :action 'take-teal-5711
			:preconds '(eled-3000 eled-3001)
			:add-list '(teal-5711))
		(make-op :action 'take-teal-5730
			:add-list '(teal-5730))
		(make-op :action 'take-teal-5735
			:add-list '(teal-5735))
		(make-op :action 'take-teal-5745
			:add-list '(teal-5745))
		(make-op :action 'take-teal-5760
			:add-list '(teal-5760))
		(make-op :action 'take-teal-5770
			:add-list '(teal-5770))
		(make-op :action 'take-teal-5780
			:add-list '(teal-5780))
		(make-op :action 'take-tece-1000
			:add-list '(tece-1000))
		(make-op :action 'take-tece-1050
			:add-list '(tece-1050))
		(make-op :action 'take-tece-1100
			:add-list '(tece-1100))
		(make-op :action 'take-tece-1150
			:add-list '(tece-1150))
		(make-op :action 'take-tece-1200
			:add-list '(tece-1200))
		(make-op :action 'take-tece-1250
			:add-list '(tece-1250))
		(make-op :action 'take-tece-1300
			:add-list '(tece-1300))
		(make-op :action 'take-tece-1320
			:add-list '(tece-1320))
		(make-op :action 'take-tece-1420
			:add-list '(tece-1420))
		(make-op :action 'take-tece-1440
			:add-list '(tece-1440))
		(make-op :action 'take-tece-1460
			:add-list '(tece-1460))
		(make-op :action 'take-tece-1480
			:add-list '(tece-1480))
		(make-op :action 'take-tece-1500
			:add-list '(tece-1500))
		(make-op :action 'take-tece-1620
			:add-list '(tece-1620))
		(make-op :action 'take-tece-1700
			:add-list '(tece-1700))
		(make-op :action 'take-tece-1750
			:add-list '(tece-1750))
		(make-op :action 'take-tece-1800
			:add-list '(tece-1800))
		(make-op :action 'take-tee-1000
			:add-list '(tee-1000))
		(make-op :action 'take-tee-1010
			:add-list '(tee-1010))
		(make-op :action 'take-tee-1020
			:add-list '(tee-1020))
		(make-op :action 'take-tee-1030
			:add-list '(tee-1030))
		(make-op :action 'take-tee-1040
			:add-list '(tee-1040))
		(make-op :action 'take-tee-1200
			:preconds '(tesy-1200)
			:add-list '(tee-1200))
		(make-op :action 'take-tee-1640
			:add-list '(tee-1640))
		(make-op :action 'take-tee-2000
			:add-list '(tee-2000))
		(make-op :action 'take-tee-2030
			:add-list '(tee-2030))
		(make-op :action 'take-tee-2200
			:add-list '(tee-2200))
		(make-op :action 'take-tee-2220
			:preconds '(tee-1200 math-1050)
			:add-list '(tee-2220))
		(make-op :action 'take-tee-2240
			:preconds '(tee-2310 tee-2400)
			:add-list '(tee-2240))
		(make-op :action 'take-tee-2300
			:add-list '(tee-2300))
		(make-op :action 'take-tee-2310
			:preconds '(tee-2300 tee-2300)
			:add-list '(tee-2310))
		(make-op :action 'take-tee-2320
			:preconds '(tee-2300)
			:add-list '(tee-2320))
		(make-op :action 'take-tee-2360
			:preconds '(tee-2300)
			:add-list '(tee-2360))
		(make-op :action 'take-tee-2370
			:preconds '(tee-2300)
			:add-list '(tee-2370))
		(make-op :action 'take-tee-2400
			:preconds '(tee-2310)
			:add-list '(tee-2400))
		(make-op :action 'take-tee-2660
			:preconds '(math-1060 tee-1200)
			:add-list '(tee-2660))
		(make-op :action 'take-tee-2850
			:preconds '(math-1050 math-1060)
			:add-list '(tee-2850))
		(make-op :action 'take-tee-3030
			:preconds '(tee-1030 tesy-1030 tee-1200 tesy-1200 mae-1200 eddt-2620 tesy-3030)
			:add-list '(tee-3030))
		(make-op :action 'take-tee-3040
			:add-list '(tee-3040))
		(make-op :action 'take-tee-3050
			:add-list '(tee-3050))
		(make-op :action 'take-tee-3070
			:add-list '(tee-3070))
		(make-op :action 'take-tee-3200
			:preconds '(tee-1000 tee-3300)
			:add-list '(tee-3200))
		(make-op :action 'take-tee-3230
			:preconds '(tee-1200 math-1050)
			:add-list '(tee-3230))
		(make-op :action 'take-tee-3240
			:preconds '(tee-1200)
			:add-list '(tee-3240))
		(make-op :action 'take-tee-3270
			:preconds '(tee-1200 tesy-1200 tesy-3270)
			:add-list '(tee-3270))
		(make-op :action 'take-tee-3300
			:preconds '(tee-1000 tee-3200)
			:add-list '(tee-3300))
		(make-op :action 'take-tee-3380
			:preconds '(tee-2240 tee-2370)
			:add-list '(tee-3380))
		(make-op :action 'take-tee-3390
			:preconds '(tee-3380)
			:add-list '(tee-3390))
		(make-op :action 'take-tee-3400
			:preconds '(tee-2300 tee-2400)
			:add-list '(tee-3400))
		(make-op :action 'take-tee-3440
			:preconds '(aste-3440)
			:add-list '(tee-3440))
		(make-op :action 'take-tee-3510
			:add-list '(tee-3510))
		(make-op :action 'take-tee-3710
			:preconds '(tee-2320)
			:add-list '(tee-3710))
		(make-op :action 'take-tee-3740
			:add-list '(tee-3740))
		(make-op :action 'take-tee-3900
			:add-list '(tee-3900))
		(make-op :action 'take-tee-3930
			:add-list '(tee-3930))
		(make-op :action 'take-tee-4210
			:preconds '(aste-4210 bsed-4210 fcse-4210)
			:add-list '(tee-4210))
		(make-op :action 'take-tee-4230
			:preconds '(tee-1030 tesy-1030 tesy-4230)
			:add-list '(tee-4230))
		(make-op :action 'take-tee-4300
			:preconds '(tee-3200 tee-3300 tee-4400)
			:add-list '(tee-4300))
		(make-op :action 'take-tee-4310
			:preconds '(chem-1110 math-1060 tee-6310)
			:add-list '(tee-4310))
		(make-op :action 'take-tee-4400
			:preconds '(tee-3200 tee-3300 tee-4300)
			:add-list '(tee-4400))
		(make-op :action 'take-tee-4440
			:preconds '(aste-6440 tee-6440)
			:add-list '(tee-4440))
		(make-op :action 'take-tee-4700
			:add-list '(tee-4700))
		(make-op :action 'take-tee-4710
			:preconds '(tee-3710)
			:add-list '(tee-4710))
		(make-op :action 'take-tee-4930
			:add-list '(tee-4930))
		(make-op :action 'take-tee-4940
			:add-list '(tee-4940))
		(make-op :action 'take-tee-5040
			:preconds '(tee-1030)
			:add-list '(tee-5040))
		(make-op :action 'take-tee-5200
			:preconds '(tee-2200)
			:add-list '(tee-5200))
		(make-op :action 'take-tee-5220
			:preconds '(tee-3200 tee-3300)
			:add-list '(tee-5220))
		(make-op :action 'take-tee-5230
			:add-list '(tee-5230))
		(make-op :action 'take-tee-5240
			:add-list '(tee-5240))
		(make-op :action 'take-tee-5500
			:preconds '(tee-5630)
			:add-list '(tee-5500))
		(make-op :action 'take-tee-5630
			:preconds '(tee-5500)
			:add-list '(tee-5630))
		(make-op :action 'take-tee-5800
			:add-list '(tee-5800))
		(make-op :action 'take-tee-5900
			:add-list '(tee-5900))
		(make-op :action 'take-tee-5910
			:add-list '(tee-5910))
		(make-op :action 'take-tee-5920
			:add-list '(tee-5920))
		(make-op :action 'take-tesy-1020
			:add-list '(tesy-1020))
		(make-op :action 'take-tesy-1030
			:add-list '(tesy-1030))
		(make-op :action 'take-tesy-1200
			:preconds '(tee-1200)
			:add-list '(tesy-1200))
		(make-op :action 'take-tesy-2250
			:add-list '(tesy-2250))
		(make-op :action 'take-tesy-2940
			:add-list '(tesy-2940))
		(make-op :action 'take-tesy-3000
			:add-list '(tesy-3000))
		(make-op :action 'take-tesy-3020
			:add-list '(tesy-3020))
		(make-op :action 'take-tesy-3030
			:preconds '(tee-1030 tesy-1030 tee-1200 tesy-1200 mae-1200 eddt-2620 tee-3030)
			:add-list '(tesy-3030))
		(make-op :action 'take-tesy-3040
			:preconds '(tesy-1200)
			:add-list '(tesy-3040))
		(make-op :action 'take-tesy-3100
			:add-list '(tesy-3100))
		(make-op :action 'take-tesy-3120
			:add-list '(tesy-3120))
		(make-op :action 'take-tesy-3200
			:preconds '(tee-1200 mae-1200)
			:add-list '(tesy-3200))
		(make-op :action 'take-tesy-3210
			:add-list '(tesy-3210))
		(make-op :action 'take-tesy-3270
			:preconds '(tee-1200 tesy-1200 tee-3270)
			:add-list '(tesy-3270))
		(make-op :action 'take-tesy-3300
			:add-list '(tesy-3300))
		(make-op :action 'take-tesy-3900
			:add-list '(tesy-3900))
		(make-op :action 'take-tesy-4000
			:add-list '(tesy-4000))
		(make-op :action 'take-tesy-4230
			:preconds '(tee-1030 tesy-1030 tee-4230)
			:add-list '(tesy-4230))
		(make-op :action 'take-tesy-4250
			:add-list '(tesy-4250))
		(make-op :action 'take-tesy-4300
			:preconds '(tee-1200)
			:add-list '(tesy-4300))
		(make-op :action 'take-tesy-4330
			:add-list '(tesy-4330))
		(make-op :action 'take-tesy-4410
			:add-list '(tesy-4410))
		(make-op :action 'take-tesy-4420
			:add-list '(tesy-4420))
		(make-op :action 'take-tesy-4430
			:add-list '(tesy-4430))
		(make-op :action 'take-tesy-4440
			:add-list '(tesy-4440))
		(make-op :action 'take-tesy-4450
			:add-list '(tesy-4450))
		(make-op :action 'take-tesy-4500
			:add-list '(tesy-4500))
		(make-op :action 'take-tesy-4510
			:add-list '(tesy-4510))
		(make-op :action 'take-tesy-4520
			:add-list '(tesy-4520))
		(make-op :action 'take-tesy-4530
			:add-list '(tesy-4530))
		(make-op :action 'take-tesy-4540
			:preconds '(tesy-4500)
			:add-list '(tesy-4540))
		(make-op :action 'take-tesy-4720
			:add-list '(tesy-4720))
		(make-op :action 'take-tesy-4730
			:add-list '(tesy-4730))
		(make-op :action 'take-tesy-4740
			:add-list '(tesy-4740))
		(make-op :action 'take-tesy-4750
			:add-list '(tesy-4750))
		(make-op :action 'take-tesy-4810
			:add-list '(tesy-4810))
		(make-op :action 'take-tesy-4820
			:add-list '(tesy-4820))
		(make-op :action 'take-tesy-4830
			:add-list '(tesy-4830))
		(make-op :action 'take-tesy-4840
			:add-list '(tesy-4840))
		(make-op :action 'take-tesy-4850
			:add-list '(tesy-4850))
		(make-op :action 'take-tesy-4900
			:preconds '(tesy-3020)
			:add-list '(tesy-4900))
		(make-op :action 'take-tesy-4940
			:add-list '(tesy-4940))
		(make-op :action 'take-tesy-5910
			:add-list '(tesy-5910))
		(make-op :action 'take-thea-1000
			:add-list '(thea-1000))
		(make-op :action 'take-thea-1010
			:preconds '(thea-1000 thea-1713)
			:add-list '(thea-1010))
		(make-op :action 'take-thea-1013
			:add-list '(thea-1013))
		(make-op :action 'take-thea-1023
			:add-list '(thea-1023))
		(make-op :action 'take-thea-1033
			:add-list '(thea-1033))
		(make-op :action 'take-thea-1050
			:add-list '(thea-1050))
		(make-op :action 'take-thea-1113
			:preconds '(thea-1000 thea-1713)
			:add-list '(thea-1113))
		(make-op :action 'take-thea-1210
			:preconds '(thea-1000 thea-1713)
			:add-list '(thea-1210))
		(make-op :action 'take-thea-1223
			:add-list '(thea-1223))
		(make-op :action 'take-thea-1310
			:add-list '(thea-1310))
		(make-op :action 'take-thea-1513
			:add-list '(thea-1513))
		(make-op :action 'take-thea-1713
			:add-list '(thea-1713))
		(make-op :action 'take-thea-2000
			:preconds '(thea-1000 thea-1010)
			:add-list '(thea-2000))
		(make-op :action 'take-thea-2010
			:preconds '(thea-2000)
			:add-list '(thea-2010))
		(make-op :action 'take-thea-2033
			:preconds '(thea-1033 thea-1033)
			:add-list '(thea-2033))
		(make-op :action 'take-thea-2100
			:preconds '(thea-1113 thea-1713)
			:add-list '(thea-2100))
		(make-op :action 'take-thea-2110
			:preconds '(thea-2100)
			:add-list '(thea-2110))
		(make-op :action 'take-thea-2200
			:preconds '(thea-1210)
			:add-list '(thea-2200))
		(make-op :action 'take-thea-2203
			:add-list '(thea-2203))
		(make-op :action 'take-thea-2210
			:preconds '(thea-2200)
			:add-list '(thea-2210))
		(make-op :action 'take-thea-2220
			:preconds '(thea-1210)
			:add-list '(thea-2220))
		(make-op :action 'take-thea-2370
			:preconds '(thea-1713 thea-1713)
			:add-list '(thea-2370))
		(make-op :action 'take-thea-2500
			:add-list '(thea-2500))
		(make-op :action 'take-thea-2510
			:add-list '(thea-2510))
		(make-op :action 'take-thea-2520
			:preconds '(thea-1513 thea-1713)
			:add-list '(thea-2520))
		(make-op :action 'take-thea-2530
			:add-list '(thea-2530))
		(make-op :action 'take-thea-2540
			:preconds '(thea-1513 thea-1713)
			:add-list '(thea-2540))
		(make-op :action 'take-thea-2550
			:preconds '(thea-1000 thea-1033 thea-1513 thea-1713)
			:add-list '(thea-2550))
		(make-op :action 'take-thea-2555
			:preconds '(thea-1513 thea-2203)
			:add-list '(thea-2555))
		(make-op :action 'take-thea-2560
			:preconds '(thea-1513 thea-1713)
			:add-list '(thea-2560))
		(make-op :action 'take-thea-2570
			:preconds '(thea-1513)
			:add-list '(thea-2570))
		(make-op :action 'take-thea-2575
			:add-list '(thea-2575))
		(make-op :action 'take-thea-2580
			:preconds '(thea-2203)
			:add-list '(thea-2580))
		(make-op :action 'take-thea-2610
			:preconds '(thea-1000 thea-1033 thea-1713)
			:add-list '(thea-2610))
		(make-op :action 'take-thea-2666
			:add-list '(thea-2666))
		(make-op :action 'take-thea-2670
			:add-list '(thea-2670))
		(make-op :action 'take-thea-2710
			:add-list '(thea-2710))
		(make-op :action 'take-thea-2910
			:add-list '(thea-2910))
		(make-op :action 'take-thea-2977
			:add-list '(thea-2977))
		(make-op :action 'take-thea-2988
			:add-list '(thea-2988))
		(make-op :action 'take-thea-3000
			:preconds '(thea-2010)
			:add-list '(thea-3000))
		(make-op :action 'take-thea-3010
			:preconds '(thea-3000)
			:add-list '(thea-3010))
		(make-op :action 'take-thea-3030
			:preconds '(thea-3000)
			:add-list '(thea-3030))
		(make-op :action 'take-thea-3100
			:preconds '(thea-2110)
			:add-list '(thea-3100))
		(make-op :action 'take-thea-3200
			:preconds '(thea-1210)
			:add-list '(thea-3200))
		(make-op :action 'take-thea-3210
			:preconds '(thea-2200)
			:add-list '(thea-3210))
		(make-op :action 'take-thea-3300
			:add-list '(thea-3300))
		(make-op :action 'take-thea-3320
			:preconds '(eled-3000 eled-3001)
			:add-list '(thea-3320))
		(make-op :action 'take-thea-3330
			:add-list '(thea-3330))
		(make-op :action 'take-thea-3340
			:add-list '(thea-3340))
		(make-op :action 'take-thea-3380
			:add-list '(thea-3380))
		(make-op :action 'take-thea-3500
			:preconds '(thea-1513 thea-2500)
			:add-list '(thea-3500))
		(make-op :action 'take-thea-3550
			:preconds '(thea-1513 thea-1713)
			:add-list '(thea-3550))
		(make-op :action 'take-thea-3560
			:add-list '(thea-3560))
		(make-op :action 'take-thea-3570
			:add-list '(thea-3570))
		(make-op :action 'take-thea-3580
			:add-list '(thea-3580))
		(make-op :action 'take-thea-3610
			:preconds '(thea-1713 thea-1033 thea-1000)
			:add-list '(thea-3610))
		(make-op :action 'take-thea-3710
			:add-list '(thea-3710))
		(make-op :action 'take-thea-3720
			:add-list '(thea-3720))
		(make-op :action 'take-thea-4000
			:preconds '(thea-3010 thea-3030)
			:add-list '(thea-4000))
		(make-op :action 'take-thea-4010
			:preconds '(thea-3030)
			:add-list '(thea-4010))
		(make-op :action 'take-thea-4020
			:preconds '(thea-4010)
			:add-list '(thea-4020))
		(make-op :action 'take-thea-4200
			:preconds '(thea-2200)
			:add-list '(thea-4200))
		(make-op :action 'take-thea-4300
			:preconds '(thea-3300)
			:add-list '(thea-4300))
		(make-op :action 'take-thea-4320
			:preconds '(thea-6320)
			:add-list '(thea-4320))
		(make-op :action 'take-thea-4340
			:preconds '(thea-3340)
			:add-list '(thea-4340))
		(make-op :action 'take-thea-4350
			:preconds '(thea-1713)
			:add-list '(thea-4350))
		(make-op :action 'take-thea-4360
			:preconds '(thea-3380 thea-3340 thea-4340)
			:add-list '(thea-4360))
		(make-op :action 'take-thea-4520
			:preconds '(thea-2520)
			:add-list '(thea-4520))
		(make-op :action 'take-thea-4540
			:preconds '(thea-2540)
			:add-list '(thea-4540))
		(make-op :action 'take-thea-4550
			:add-list '(thea-4550))
		(make-op :action 'take-thea-4555
			:preconds '(thea-1513 thea-1713 thea-2203)
			:add-list '(thea-4555))
		(make-op :action 'take-thea-4560
			:preconds '(thea-2500 thea-2560)
			:add-list '(thea-4560))
		(make-op :action 'take-thea-4710
			:preconds '(thea-3710 thea-3720)
			:add-list '(thea-4710))
		(make-op :action 'take-thea-4720
			:add-list '(thea-4720))
		(make-op :action 'take-thea-4730
			:preconds '(thea-4720)
			:add-list '(thea-4730))
		(make-op :action 'take-thea-4760
			:preconds '(thea-1713 engl-2010 engl-4250)
			:add-list '(thea-4760))
		(make-op :action 'take-thea-4820
			:preconds '(thea-1000 thea-1033 thea-1713)
			:add-list '(thea-4820))
		(make-op :action 'take-thea-4860
			:preconds '(thea-4760 engl-4250)
			:add-list '(thea-4860))
		(make-op :action 'take-thea-5310
			:add-list '(thea-5310))
		(make-op :action 'take-thea-5390
			:add-list '(thea-5390))
		(make-op :action 'take-thea-5550
			:preconds '(thea-2560 thea-2520)
			:add-list '(thea-5550))
		(make-op :action 'take-thea-5590
			:preconds '(thea-4560 thea-4520 thea-4540 thea-6590)
			:add-list '(thea-5590))
		(make-op :action 'take-thea-5610
			:preconds '(thea-3610 thea-3710 thea-3720)
			:add-list '(thea-5610))
		(make-op :action 'take-thea-5630
			:add-list '(thea-5630))
		(make-op :action 'take-thea-5631
			:preconds '(thea-5390)
			:add-list '(thea-5631))
		(make-op :action 'take-thea-5720
			:preconds '(thea-3710 thea-3720)
			:add-list '(thea-5720))
		(make-op :action 'take-thea-5900
			:preconds '(thea-1513 thea-1713 thea-2203)
			:add-list '(thea-5900))
		(make-op :action 'take-thea-5910
			:add-list '(thea-5910))
		(make-op :action 'take-thea-5940
			:add-list '(thea-5940))
		(make-op :action 'take-thea-5960
			:preconds '(thea-6960)
			:add-list '(thea-5960))
		(make-op :action 'take-thea-5980
			:preconds '(thea-6980)
			:add-list '(thea-5980))
		(make-op :action 'take-usu-1010
			:add-list '(usu-1010))
		(make-op :action 'take-usu-1020
			:add-list '(usu-1020))
		(make-op :action 'take-usu-1030
			:add-list '(usu-1030))
		(make-op :action 'take-usu-1040
			:add-list '(usu-1040))
		(make-op :action 'take-usu-1045
			:add-list '(usu-1045))
		(make-op :action 'take-usu-1050
			:preconds '(usu-1010)
			:add-list '(usu-1050))
		(make-op :action 'take-usu-1060
			:add-list '(usu-1060))
		(make-op :action 'take-usu-1070
			:add-list '(usu-1070))
		(make-op :action 'take-usu-1080
			:add-list '(usu-1080))
		(make-op :action 'take-usu-1150
			:preconds '(usu-1050 usu-1050)
			:add-list '(usu-1150))
		(make-op :action 'take-usu-1300
			:add-list '(usu-1300))
		(make-op :action 'take-usu-1320
			:add-list '(usu-1320))
		(make-op :action 'take-usu-1330
			:add-list '(usu-1330))
		(make-op :action 'take-usu-1340
			:add-list '(usu-1340))
		(make-op :action 'take-usu-1350
			:add-list '(usu-1350))
		(make-op :action 'take-usu-1360
			:add-list '(usu-1360))
		(make-op :action 'take-usu-1400
			:add-list '(usu-1400))
		(make-op :action 'take-usu-1730
			:add-list '(usu-1730))
		(make-op :action 'take-usu-1750
			:add-list '(usu-1750))
		(make-op :action 'take-usu-2150
			:add-list '(usu-2150))
		(make-op :action 'take-usu-2170
			:add-list '(usu-2170))
		(make-op :action 'take-usu-2250
			:add-list '(usu-2250))
		(make-op :action 'take-usu-2400
			:add-list '(usu-2400))
		(make-op :action 'take-usu-3070
			:preconds '(honr-3070)
			:add-list '(usu-3070))
		(make-op :action 'take-usu-3071
			:add-list '(usu-3071))
		(make-op :action 'take-usu-3072
			:add-list '(usu-3072))
		(make-op :action 'take-usu-3400
			:add-list '(usu-3400))
		(make-op :action 'take-usu-3410
			:add-list '(usu-3410))
		(make-op :action 'take-usu-4000
			:add-list '(usu-4000))
		(make-op :action 'take-usu-4250
			:add-list '(usu-4250))
		(make-op :action 'take-usu-4910
			:preconds '(usu-4000)
			:add-list '(usu-4910))
		(make-op :action 'take-wats-1200
			:add-list '(wats-1200))
		(make-op :action 'take-wats-2000
			:preconds '(envs-2000 wild-2000)
			:add-list '(wats-2000))
		(make-op :action 'take-wats-2220
			:preconds '(biol-1610 biol-1620 biol-1620)
			:add-list '(wats-2220))
		(make-op :action 'take-wats-2500
			:preconds '(biol-1620)
			:add-list '(wats-2500))
		(make-op :action 'take-wats-2800
			:add-list '(wats-2800))
		(make-op :action 'take-wats-3000
			:add-list '(wats-3000))
		(make-op :action 'take-wats-3050
			:add-list '(wats-3050))
		(make-op :action 'take-wats-3060
			:preconds '(wats-3050)
			:add-list '(wats-3060))
		(make-op :action 'take-wats-3100
			:preconds '(biol-1010 biol-1610 biol-1620)
			:add-list '(wats-3100))
		(make-op :action 'take-wats-3110
			:preconds '(wats-3100)
			:add-list '(wats-3110))
		(make-op :action 'take-wats-3450
			:add-list '(wats-3450))
		(make-op :action 'take-wats-3600
			:preconds '(math-1060 math-1100 math-1210 phys-2210 geo-3600)
			:add-list '(wats-3600))
		(make-op :action 'take-wats-3700
			:add-list '(wats-3700))
		(make-op :action 'take-wats-3910
			:preconds '(math-1050 phys-2110 phys-2210 math-1100 math-1210)
			:add-list '(wats-3910))
		(make-op :action 'take-wats-4000
			:preconds '(wats-6000)
			:add-list '(wats-4000))
		(make-op :action 'take-wats-4110
			:preconds '(chem-1210 chem-1215 chem-1220 chem-1225 wats-2220 wats-3700 wats-6110)
			:add-list '(wats-4110))
		(make-op :action 'take-wats-4210
			:preconds '(wats-6210)
			:add-list '(wats-4210))
		(make-op :action 'take-wats-4250
			:add-list '(wats-4250))
		(make-op :action 'take-wats-4310
			:preconds '(wats-3700 wats-6310)
			:add-list '(wats-4310))
		(make-op :action 'take-wats-4490
			:preconds '(math-1100 math-1210 wats-3700 wats-6490)
			:add-list '(wats-4490))
		(make-op :action 'take-wats-4500
			:preconds '(chem-1210 chem-1215 chem-1220 chem-1225 wats-2220 wats-3700 wats-6500)
			:add-list '(wats-4500))
		(make-op :action 'take-wats-4510
			:preconds '(chem-1210 chem-1220 wats-4500)
			:add-list '(wats-4510))
		(make-op :action 'take-wats-4520
			:add-list '(wats-4520))
		(make-op :action 'take-wats-4530
			:preconds '(wats-6530)
			:add-list '(wats-4530))
		(make-op :action 'take-wats-4540
			:preconds '(biol-1010 biol-1610 biol-1620 wats-6540)
			:add-list '(wats-4540))
		(make-op :action 'take-wats-4580
			:preconds '(wats-6580)
			:add-list '(wats-4580))
		(make-op :action 'take-wats-4600
			:preconds '(math-1100 math-1210 wats-6600)
			:add-list '(wats-4600))
		(make-op :action 'take-wats-4650
			:preconds '(wats-6650)
			:add-list '(wats-4650))
		(make-op :action 'take-wats-4720
			:preconds '(wats-6720)
			:add-list '(wats-4720))
		(make-op :action 'take-wats-4930
			:preconds '(geog-2800 wats-6920)
			:add-list '(wats-4930))
		(make-op :action 'take-wats-4931
			:preconds '(wats-4930 wats-6920 wats-6921)
			:add-list '(wats-4931))
		(make-op :action 'take-wats-4950
			:add-list '(wats-4950))
		(make-op :action 'take-wats-4960
			:add-list '(wats-4960))
		(make-op :action 'take-wats-4970
			:add-list '(wats-4970))
		(make-op :action 'take-wats-4980
			:preconds '(wats-6800 wats-7800)
			:add-list '(wats-4980))
		(make-op :action 'take-wats-4990
			:add-list '(wats-4990))
		(make-op :action 'take-wats-5003
			:preconds '(math-1100 math-1210 phys-2110 phys-2210 wats-6003)
			:add-list '(wats-5003))
		(make-op :action 'take-wats-5150
			:preconds '(geo-5150 wats-6150)
			:add-list '(wats-5150))
		(make-op :action 'take-wats-5200
			:add-list '(wats-5200))
		(make-op :action 'take-wats-5300
			:preconds '(wats-2220 biol-2220)
			:add-list '(wats-5300))
		(make-op :action 'take-wats-5310
			:preconds '(biol-2220 wats-2220 wats-5300)
			:add-list '(wats-5310))
		(make-op :action 'take-wats-5340
			:preconds '(geog-2800 wild-2800 wats-5300)
			:add-list '(wats-5340))
		(make-op :action 'take-wats-5350
			:preconds '(wats-5340)
			:add-list '(wats-5350))
		(make-op :action 'take-wats-5550
			:preconds '(biol-5550)
			:add-list '(wats-5550))
		(make-op :action 'take-wats-5610
			:add-list '(wats-5610))
		(make-op :action 'take-wats-5620
			:add-list '(wats-5620))
		(make-op :action 'take-wats-5621
			:add-list '(wats-5621))
		(make-op :action 'take-wats-5622
			:add-list '(wats-5622))
		(make-op :action 'take-wats-5623
			:add-list '(wats-5623))
		(make-op :action 'take-wats-5624
			:add-list '(wats-5624))
		(make-op :action 'take-wats-5625
			:add-list '(wats-5625))
		(make-op :action 'take-wats-5630
			:add-list '(wats-5630))
		(make-op :action 'take-wats-5640
			:preconds '(wats-2220 biol-2220 wats-3700 wats-7640)
			:add-list '(wats-5640))
		(make-op :action 'take-wats-5650
			:add-list '(wats-5650))
		(make-op :action 'take-wats-5660
			:add-list '(wats-5660))
		(make-op :action 'take-wats-5670
			:add-list '(wats-5670))
		(make-op :action 'take-wats-5680
			:preconds '(geo-3600 wats-3600 geo-5680 psc-5680 wats-6680)
			:add-list '(wats-5680))
		(make-op :action 'take-weld-1010
			:add-list '(weld-1010))
		(make-op :action 'take-weld-1100
			:add-list '(weld-1100))
		(make-op :action 'take-weld-1110
			:add-list '(weld-1110))
		(make-op :action 'take-weld-1120
			:add-list '(weld-1120))
		(make-op :action 'take-weld-1130
			:add-list '(weld-1130))
		(make-op :action 'take-weld-1140
			:add-list '(weld-1140))
		(make-op :action 'take-weld-1150
			:add-list '(weld-1150))
		(make-op :action 'take-weld-1160
			:add-list '(weld-1160))
		(make-op :action 'take-weld-1170
			:preconds '(weld-1110)
			:add-list '(weld-1170))
		(make-op :action 'take-weld-1500
			:add-list '(weld-1500))
		(make-op :action 'take-weld-2400
			:add-list '(weld-2400))
		(make-op :action 'take-weld-2410
			:add-list '(weld-2410))
		(make-op :action 'take-weld-2500
			:add-list '(weld-2500))
		(make-op :action 'take-weld-2600
			:add-list '(weld-2600))
		(make-op :action 'take-weld-2977
			:add-list '(weld-2977))
		(make-op :action 'take-weld-2988
			:add-list '(weld-2988))
		(make-op :action 'take-wild-2000
			:preconds '(envs-2000 wats-2000)
			:add-list '(wild-2000))
		(make-op :action 'take-wild-2200
			:add-list '(wild-2200))
		(make-op :action 'take-wild-2250
			:add-list '(wild-2250))
		(make-op :action 'take-wild-2400
			:preconds '(math-1050 biol-1610 biol-1620)
			:add-list '(wild-2400))
		(make-op :action 'take-wild-2800
			:add-list '(wild-2800))
		(make-op :action 'take-wild-3100
			:add-list '(wild-3100))
		(make-op :action 'take-wild-3300
			:preconds '(biol-1610 biol-1620 biol-2220 wats-2220)
			:add-list '(wild-3300))
		(make-op :action 'take-wild-3500
			:add-list '(wild-3500))
		(make-op :action 'take-wild-3600
			:add-list '(wild-3600))
		(make-op :action 'take-wild-3800
			:preconds '(biol-1620 wats-2220 biol-2220)
			:add-list '(wild-3800))
		(make-op :action 'take-wild-3810
			:preconds '(wats-2220 biol-2220 math-1100 stat-2000 stat-3000)
			:add-list '(wild-3810))
		(make-op :action 'take-wild-3820
			:preconds '(math-1050)
			:add-list '(wild-3820))
		(make-op :action 'take-wild-3830
			:add-list '(wild-3830))
		(make-op :action 'take-wild-3850
			:preconds '(wild-3800)
			:add-list '(wild-3850))
		(make-op :action 'take-wild-4000
			:add-list '(wild-4000))
		(make-op :action 'take-wild-4100
			:preconds '(engl-2010 wild-3800)
			:add-list '(wild-4100))
		(make-op :action 'take-wild-4250
			:add-list '(wild-4250))
		(make-op :action 'take-wild-4300
			:preconds '(wild-6300)
			:add-list '(wild-4300))
		(make-op :action 'take-wild-4340
			:preconds '(wild-6340)
			:add-list '(wild-4340))
		(make-op :action 'take-wild-4500
			:preconds '(wild-3810)
			:add-list '(wild-4500))
		(make-op :action 'take-wild-4550
			:add-list '(wild-4550))
		(make-op :action 'take-wild-4570
			:preconds '(wild-6570)
			:add-list '(wild-4570))
		(make-op :action 'take-wild-4600
			:preconds '(wats-2220 biol-2220)
			:add-list '(wild-4600))
		(make-op :action 'take-wild-4640
			:add-list '(wild-4640))
		(make-op :action 'take-wild-4700
			:preconds '(biol-1610 biol-1620 wats-2220 biol-2220)
			:add-list '(wild-4700))
		(make-op :action 'take-wild-4750
			:preconds '(biol-2220 wats-2220 math-1100 stat-2000 stat-3000 wild-2400 wild-3810)
			:add-list '(wild-4750))
		(make-op :action 'take-wild-4880
			:preconds '(chem-1110 chem-1210 biol-1610)
			:add-list '(wild-4880))
		(make-op :action 'take-wild-4910
			:preconds '(wild-2400 wild-3800 wild-3810 wild-4750)
			:add-list '(wild-4910))
		(make-op :action 'take-wild-4950
			:add-list '(wild-4950))
		(make-op :action 'take-wild-4960
			:add-list '(wild-4960))
		(make-op :action 'take-wild-4970
			:add-list '(wild-4970))
		(make-op :action 'take-wild-4980
			:add-list '(wild-4980))
		(make-op :action 'take-wild-5220
			:preconds '(wild-7220)
			:add-list '(wild-5220))
		(make-op :action 'take-wild-5300
			:preconds '(wild-7300)
			:add-list '(wild-5300))
		(make-op :action 'take-wild-5350
			:preconds '(chem-1110 psc-3000 psc-5350 wild-6350)
			:add-list '(wild-5350))
		(make-op :action 'take-wild-5560
			:preconds '(biol-1620 wats-2220 biol-2220 engl-2010 wild-6560)
			:add-list '(wild-5560))
		(make-op :action 'take-wild-5580
			:preconds '(biol-1620 biol-1625 biol-5580)
			:add-list '(wild-5580))
		(make-op :action 'take-wild-5700
			:add-list '(wild-5700))
		(make-op :action 'take-wild-5710
			:add-list '(wild-5710))
		(make-op :action 'take-wild-5750
			:preconds '(wild-6750)
			:add-list '(wild-5750))
		(make-op :action 'take-wild-5860
			:preconds '(advs-5860)
			:add-list '(wild-5860))))
(mapc #'convert-op *registration-ops*)
(provide :ops)
