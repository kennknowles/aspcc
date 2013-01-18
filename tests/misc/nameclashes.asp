<%
	class foo
		public default property get wammo(num)
			wammo = num * num
		end property
	end class

	function sq(num)
		dim foo ' here it is ok, because local scope gets weird priviledges
		sq = num + num
	end function

	dim foo ' they also don't let class names overlap
	dim sq ' in reality, vbscript doesn't let functions and variables collide
			' so what I'll do is use the variable
	set sq = new foo

	response.write sq(4)
%>
