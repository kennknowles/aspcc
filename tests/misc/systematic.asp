<html>
<body>
<h1>This is the page</h1>

This is html content

<%
	' CALL
	call assmonkey(3)
	call ass.monkey(5)
	call fart()
	call eat().my.baaalls()

	' CLASS
	class my_class

		dim barf
		dim wahoodle()

		private donkeys
		private donkey_lovers(4,5)

		public function bark_at_the_moon(length, loudness)
			length = loudness
			exit function
		end function

		public function again()
			call rock_out()
		end function

		public property get dickmonkey()
			dickmonkey = barf
		end property

		public property set dickmonkey(hello)
			barf = dickmonkey
		end property

	end class


	' CONST	
	const arf = 4
	const wacky = free.willy("hello")

	' DO
	do while goin
		call eat_it(2,5, 6)
	loop

	do until barf
		call shitbrick()
	loop

	do
		dim jack_shit()
	loop while shitfaced

	do
		dim po(2,5)
	loop until nothing

	' ERASE
	erase yourmom
	erase yourdad.yoursister
	erase yourdads("third")

	' EXECUTE
	execute ( "Some code" )

	' EXIT
	do
		x = 5
		exit do
	loop while x

	' FOR
	for i = 1 to 5
		call test1.test2(1,2,6)
		exit for
	next

	for i = 12 to 1 step -1
		erase test1.test2
		erase test1
	next

	' FOR EACH
	for each garbage in the_trash
		do while garbage.ugly()
			erase garbage
		loop
	next

	' FUNCTION
	function test_func(param1, param2)
		test_func = "hello"
		another_var = "goodbye"
		exit function
	end function

	
	' IF
	if hello then
		a = 3
	else
		a = 5
	end if


	' ON ERROR / OPTION EXPLICIT
	on error resume next
	on error goto 0
	option explicit

	' SELECT
	select case hello
	case 45, 50, 69
		z = 89
	case "word"
		z = 343
	case else
		z = 100
	end select

	' SET
	set x = y
	set z = y.x
	set x.froofroo = y.z().zabnab(2.4, 58)

	sub dance_around
		call fart_knockin()
	end sub

	while 5 
		call smelly(2,6,9)
	wend

	while big.bad.object.access.dude()
		if yourmom then
			call dance_around()
			set i = 4
		end if
		exit while
	wend

	
	' Various expressions
	a = 4 + 5
	b = (5)
	c = (5 + 8 * 6)
	d = (5 ^ (24 - 5))
	e = -10
	f = -45.0
	g = -(50 \ 8)

	' first, check our booleans
	z = true
	if true and false then
		x = 1
	else
		x = 4
	end if

	if true and false then
		x = 1
	elseif (not true or false) eqv (true imp false) then
		x = 4
	end if

	' Some intrinsic objects
	call Response.write( "hey baby" )
	call Response.write( abs(3) )
%>

</body>
</html>
