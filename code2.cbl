	IDENTIFICATION DIVISION.
	PROGRAM-ID.  StudDetailsRpt.
	AUTHOR.  Michael Coughlan.
	

	ENVIRONMENT DIVISION.
	INPUT-OUTPUT SECTION.
	FILE-CONTROL.
	   SELECT UsuariosFile ASSIGN TO "USUARIOS.DAT"
		ORGANIZATION IS LINE SEQUENTIAL
		FILE STATUS IS StatUsuarios.
		   
		SELECT ArticulosFile ASSIGN TO "ART.DAT"
		ORGANIZATION IS LINE SEQUENTIAL
		FILE STATUS IS StatArticulos.
					  
					  
		SELECT AlmacenesFile ASSIGN TO "ALM.DAT"
		ORGANIZATION IS LINE SEQUENTIAL.
					  
		SELECT ReportFile ASSIGN TO "SALIDA.RPT"
        ORGANIZATION IS LINE SEQUENTIAL.

	DATA DIVISION.
	FILE SECTION.
	
	FD UsuariosFile.
	01  UsuariosDetails.
		88  EndOfUsersFile VALUE HIGH-VALUES.
		02  UserId        PIC X(8).
		02  UserName.
               03 Name       PIC X(10).
               03 Surname    PIC X(10).
	    02  Direccion	 PIC X(15).
        02  Telefono      PIC 9(9).
       
	FD ArticulosFile.
	01 ArticulosDetails.
	    88 EndOfArticulosFile VALUE HIGH-VALUES.
		02 UserArtID PIC X(8).
		02 ArtID     PIC 9(7).
		02 Cant	   PIC 9(2).
		  
	FD AlmacenesFile.
	01 AlmacenesDetails.
        88 EndOfAlmacenesFile VALUE HIGH-VALUES.
         02 ArtAlID PIC X(7).
		 02 Precio  PIC 9(4).
			 
   	FD ReportFile.
	01 PrintLine          PIC X(92).


	WORKING-STORAGE SECTION.
	   
	01 Cabecera2.
	       02 Titulo1 PIC X(29) VALUE "UserID   Name      Apellidos".
		   02 Titulo2 PIC X(28) VALUE " Calle           Telefono ".
		   02 Titulo3 PIC X(22) VALUE "ArtID  Ca Prec Total".
	   
	01 Todo.
	       02  WUserId        PIC X(8).
		   02  Sep1           PIC X VALUE " ".
	       02  WUserName.
               03 WName       PIC X(10).
               03 WSurname    PIC X(10).
			   03  Sep2           PIC X VALUE " ".
	       02  WDireccion	  PIC X(15).
		   02  Sep1           PIC X VALUE " ".
           02  WTelefono      PIC 9(9).
		   02  Sep1           PIC X VALUE " ".
		   02  WArtID         PIC 9(7).
		   02  Sep1           PIC X VALUE " ".
		   02  WCant	      PIC 9(2).
		   02  Sep1           PIC X VALUE " ".
		   02  WPrecio        PIC 9(4).
		   02  Sep1           PIC X VALUE " ".
		   02  WTotal 		  PIC 9(13).
	   
	   
	01 VariablePrecios PIC 9(13).
	
	01 TextoVariable PIC X(97).
	    
	   
	01 StatArticulos PIC X(02) VALUE SPACES.
	           88 Iniciado VALUE '00'.
			   88 Terminado VALUE '10'.
	   
	01 StatUsuarios PIC X(02) VALUE SPACES.
	           88 UsIniciado VALUE '00'.
			   88 UsTerminado VALUE '10'.
			   
	01 Encontrado PIC X VALUE SPACES.
			88 Found VALUE '1'. 
			
	01 Aux PIC 9(10).
	
	01 PageFooting.
	   02 PrnSubtotalCab  PIC X(14) VALUE SPACES.
	   02 PrnSubtotal     PIC 9(13) BLANK WHEN ZERO.
	   
	01 PageEnc.
	   02 FILLER          PIC X(10) VALUE "COGNIZANT".
	   02 FILLER          PIC X(7) VALUE "Page : ".
	   02 PrnPageNum      PIC Z9.
	 
	01 PageItems.
	   02 LineCount          PIC 99 VALUE ZEROS.
	   02 PageNum            PIC 99 VALUE 01.
	   02 Subtotal           PIC 9(13).
	   
	01 Cont PIC 99.
	
	01 Total PIC 9(13).

	PROCEDURE DIVISION.
	OPEN INPUT UsuariosFile,ArticulosFile,AlmacenesFile.
	OPEN OUTPUT Reportfile.
	
	
	Inicio.
		
		
		MOVE 5 TO LineCount.
		MOVE PageNum TO PrnPageNum.
		WRITE PrintLine FROM PageEnc
		WRITE PrintLine FROM Cabecera2 BEFORE ADVANCING 1 LINES
		
		READ UsuariosFile 
		AT END SET EndOfUsersFile TO TRUE
		END-READ
		PERFORM UNTIL EndOfUsersFile
	        
			PERFORM ArtLec 
			READ UsuariosFile 
			AT END SET EndOfUsersFile TO TRUE
			END-READ
			
			 MOVE "  Total : " TO PrnSubtotalCab
			 
			 PERFORM NuevaPagina
			 MOVE ZEROS TO Total
			 
			 
			
			CLOSE ArticulosFile
			OPEN INPUT ArticulosFile
		END-PERFORM
		
		CLOSE UsuariosFile,ArticulosFile,AlmacenesFile,ReportFile.
		STOP RUN.
	ArtLec.
		READ ArticulosFile 
		AT END SET EndOfArticulosFile TO TRUE
		END-READ
		PERFORM UNTIL EndOfArticulosFile
	        
			IF UserID=UserArtID
			    
				PERFORM Alma
			END-IF
			
			READ ArticulosFile 
			AT END SET EndOfArticulosFile TO TRUE
			END-READ
			CLOSE AlmacenesFile
			OPEN INPUT AlmacenesFile
		END-PERFORM.
   
	Alma.
		READ AlmacenesFile 
		AT END SET EndOfAlmacenesFile TO TRUE
		END-READ
		PERFORM UNTIL EndOfAlmacenesFile 
		
	       
			
			IF ArtID=ArtAlID
			    
				COMPUTE Aux = Cant * Precio
				
				   MOVE UserId TO WUserId
				   MOVE Name to WName
				   MOVE Surname to WSurname
				   MOVE Direccion to WDireccion
				   MOVE Telefono to WTelefono
				   MOVE ArtID TO WArtID
				   MOVE Cant TO WCant
				   MOVE Precio TO WPrecio
				   MOVE Aux TO WTotal 
				   SET EndOfAlmacenesFile to TRUE
				   		   
				   
				      IF LineCount = 40 
				      
				      MOVE "  Subtotal : " TO PrnSubtotalCab
					  MOVE Total TO Subtotal
					  
				      PERFORM NuevaPagina
					  
					 
					  
				   END-IF
				   
					COMPUTE Total = Total + Aux
					WRITE PrintLine FROM Todo 
					ADD 1 TO LineCount
				   
				  
				   
				   
				   
				   
			END-IF
			
			IF NOT EndOfAlmacenesFile
				READ AlmacenesFile 
				AT END SET EndOfAlmacenesFile TO TRUE
			END-READ
			END-IF
		
		END-PERFORM.
	NuevaPagina.
		
		ADD 1 TO PageNum
		MOVE PageNum TO PrnPageNum
		MOVE Total TO PrnSubtotal
		WRITE PrintLine FROM PageFooting BEFORE ADVANCING 1 LINES
		WRITE PrintLine FROM PageEnc
		WRITE PrintLine FROM Cabecera2 BEFORE ADVANCING 1 LINES
		MOVE 4 TO LineCount
		MOVE SPACES TO PrnSubtotalCab.
			
	
		
	