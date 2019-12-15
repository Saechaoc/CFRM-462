#Chris Saechao
#Midterm Winter 2002 Answers

#Q1
"What is the simple 1 month return between December, 2000 and December, 2001? Suppose you can get this return every month for the next year. What is the simple 1 year return? Compare this return to the actual 1 year simple return between December, 2000 and December, 2001."


w0 = 129.975
w1 = 135.754
w12 = 114.3

#simple 1 month return
ret_1m = (w1-w0)/w0

#Sample Y1 return
ret_12m = (1 + ret_1m)^12 - 1

#Actual 1Y Return
ret_1Y = (w12-w0)/w0

#Q2
"Suppose the consumer price index (CPI) for December 2000 is equal to 100 and that the CPI for December 2001 is 105. Compute the simple 1 year real return between December 2000 and December 2001."
 
#Return with CPI
ret_cpi = ((w12/105) - (w0/100))/(w0/100)

#Q3
"What is the continuously compounded 1 month return between December, 2000 and December, 2001? Suppose you can get this return every month for the next year. What is the continuously compounded annual year return? Compare this return to the actual 1 year continuously compounded return between December, 2000 and December, 2001. "














