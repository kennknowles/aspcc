<%
	set fs = Server.CreateObject("Scripting.FileSystemObject")
%>
<%= fs.GetParentFolderName("x/y/z.exe") %><br />

<%= fs.GetParentFolderName("x/y/z") %><br />

<%= fs.GetParentFolderName("x/y/z/") %><br />

<%= fs.GetBaseName("x/y") %><br />

<%= fs.GetBaseName("x/y/") %><br />

<%= fs.GetBaseName("x/y.exe") %><br />
