<%
Dim intLowerBound    ' Lower bound of the random number range
Dim intUpperBound    ' Upper bound of the random number range

Dim intRangeSize     ' Size of the range
Dim sngRandomValue   ' A random value from 0 to intRangeSize
Dim intRandomInteger ' Our final result - random integer to return


' Retrieve lower and upper bound requests if they're there
' o/w set to defaults of 0 and 100
If IsNumeric(Request.QueryString("lowerbound")) Then
	intLowerBound = CLng(Request.QueryString("lowerbound"))
Else
	intLowerBound = 0
End If

If IsNumeric(Request.QueryString("upperbound")) Then
	intUpperBound = CLng(Request.QueryString("upperbound"))
	
	' Add a line to deal with default case of 0 to 0.
	' This really isn't neccessary, but I do it so the
	' sample doesn't default to generating a number between
	' 0 and 0 and always return 0 when no bounds are provided.
	If intLowerBound = 0 And intUpperBound = 0 Then intUpperBound = 100
Else
	intUpperBound = 100
End If


' Check for people asking for a number from in an inappropriate
' range (ie: 50 to 10) and swap the bounds
If intLowerBound > intUpperBound Then
	' I really should've declared a temporary variable for this
	' swapping, but I was lazy and this one was already defined
	' and I don't use it till later... oh all right I'll do it
	' the "right" way... actually even this is bad... I should've
	' defined this up top... so sue me... hey it's free code what
	' do you want from me?
	Dim iTemp
	iTemp = intLowerBound
	intLowerBound = intUpperBound
	intUpperBound = iTemp
End If


' Initialize the random number generator.
' Randomize can actually take parameters telling it how to initialize
' things, but for the most you'll just want to call it without passing
' it anything.
Randomize()

' Generate our random number.
' The Rnd function does most of the work.  It returns a value in the
' range 0 <= value < 1 so to generate a random integer in the specified
' range we need to do some calculation.  Specifically we take the size
' of the range in which we want to generate the number (add 1 so the
' upper bound can be generated!) and then multiply it by our random
' element.  Then to place the value into the correct range of numbers
' we add the lower bound.  Finally we truncate the number leaving us
' with the integer portion which is always somewhere between the
' lower bound and upper bound (inclusively).

' Find range size
intRangeSize = intUpperBound - intLowerBound + 1

' Get a random number from 0 to the size of the range
sngRandomValue = intRangeSize * Rnd()

' Center the range of possible random numbers over the desired result set
sngRandomValue = sngRandomValue + intLowerBound

' Convert our value to an integer
intRandomInteger = Int(sngRandomValue)

' comments comments comments

%>
You asked for a random number between <B><%= intLowerBound %></B> and <B><%= intUpperBound %></B>.<BR>
The computer returned: <B><%= intRandomInteger %></B><BR>

<!-- Build the form for user input -->
<FORM ACTION="random_number.asp" METHOD="get" NAME="frmRandomNumberBounds">

Generate a random number between
<INPUT TYPE="text" NAME="lowerbound" VALUE="<%= intLowerBound %>" SIZE="5" MAXLENGTH="5"></INPUT>
 and 
<INPUT TYPE="text" NAME="upperbound" VALUE="<%= intUpperBound %>" SIZE="5" MAXLENGTH="5"></INPUT>

<INPUT TYPE="submit"></INPUT>

</FORM>

