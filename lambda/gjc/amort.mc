/* -*- Mode:MACSYMA; Fonts:(CPTFONTB) -*- */

eval_when([translate,batch,demo],
mdefine(funlhs,exp)::=ev(buildq([funlhs,exp],buildq([lh:funlhs,rh:ev(exp)],
lh:=rh)),eval),

/* The amortization formula is based the sum of a geometric sequence.
 */

present_value(starting_value,payment,rate,time):=
 starting_value*(rate+1)^time - payment * ((rate+1)^time-1) / rate);

/* There are two things one wants to know when considering a loan:
   (a) how much will the payments be.
   (b) what is the actual interest rate being paid once you account
       for "points" (i.e. money-up-front) and monthing compounding.
 */

mdefine(payment('starting_value,'rate,'time),
 /* We get the formula by solving for the payment assuming a present_value
    of zero. i.e. full payment of the loan by the given time */
 rhs(first(solve(present_value('starting_value,'payment,'rate,'time),
                 'payment))));


monthly_payment(starting_value,yearly_percentage_rate,years):=
 payment(starting_value,
         yearly_percentage_rate/100/12,
         12*years);

/* In the equation for present value we cannot be solve directly
   for the rate R, so instead we approximate.
 */

mdefine(rate_approx('starting_value,'payment,'rate_approx,'time),
   'rate_approx -
        present_value('starting_value,'payment,'rate_approx,'time) /
        diff(present_value('starting_value,'payment,'rate_approx,'time),
             'rate_approx));

apr(mort_amount,points,payment,stated_rate,years):=
 block([starting_value : mort_amount - mort_amount * points/100,
        mpr: stated_rate/100/12,temp],
        while ( temp:rate_approx(starting_value,
                                 payment,
                                 mpr,
                                 years*12),
                 abs(temp - mpr) > ratepsilon)
        do (print("MONTHLY PRECENTAGE APPROXIMATELY =",TEMP*100),
            mpr: temp),
        ((mpr+1)^12-1)*100);


run_loop():=
 block([ma:read("Mortgage amount"),pay,ypr,yrs,pnts,apr,ins],
       loop,
       ypr:float(read("Percentage")),
       yrs:read("Years"),
       pnts:read("Points"),
       ins:float(read("Insurance factor (or zero)")),
       print("Calculations for",ma,"at",ypr,"for",yrs,"years. with",pnts,
             "points"),
       pay:monthly_payment(ma,ypr,yrs) + ma*ins/12 ,
       print("Monthly Payment =",pay),
       apr:apr(ma,pnts,pay,ypr,yrs),
       print("Actual Rate (APR) =",apr),
       go(loop));
