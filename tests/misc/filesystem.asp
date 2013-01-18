<html>
<body>
<%
Set Sys = Server.CreateObject("Scripting.FilesystemObject")

Sys.DeleteFile("C:\www\test.txt")
Sys.Delete("C:\www\test.txt")
Sys.folderexists("C:\www\temp\")
Sys.createfolder("C:\www\temp\")

Set Txt=Sys.OpenTextFile("C:\tmp\testfile.txt",2)
Txt.Write ("hello")
blah = Txt.ReadAll
Txt.ReadAll
Txt.close

%>

</body>
</html>

