<%
	class contents
		public y

		public default property get x
			 x = y
		end property

		public property get fibble
			fibble = y
		end property

		public property let x(value)
			y = value
		end property
	end class

	class container
		public c

		public default property get z
			set z = c
		end property
	end class

	dim foo, baz

	set foo = new container
	set foo.c = new contents
	'set foo.z = new contents   DOESNT WORK - YAY!!! EASIER!!!
	foo.z.y = 3
	response.write foo.z.y

	foo.c.y = 4
	response.write foo.z.y

	foo.z.x = 5
	response.write foo.z.y

	foo.z.fibble = 7 ' also doesn't work, YAY
	response.write foo.z.y
%>
