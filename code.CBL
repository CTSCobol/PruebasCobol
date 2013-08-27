	   IDENTIFICATION DIVISION.                 
       PROGRAM-ID. TESTCOBL.                    
       AUTHOR. Miguel Martin.                        
                                 
                                                
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
					  
           SELECT ReportFile ASSIGN TO "REPORT.RPT"
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
			
		  
       FD  ReportFile. 
       01  PrintLine            PIC X(97).
	   	   
	   WORKING-STORAGE SECTION.
	   
	   01 Cabecera1 PIC X(30) VALUE "COGNIZANT TECHNOLOGY SOLUTIONS".
	   
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
	    
	   01 Bandera PIC X VALUE SPACES.
	       88 Seguir VALUE '0'.
		   88 Parar VALUE '1'.
		   
	   01 StatArticulos PIC X(02) VALUE SPACES.
	           88 Iniciado VALUE '00'.
			   88 Terminado VALUE '10'.
	   
	   01 StatUsuarios PIC X(02) VALUE SPACES.
	           88 UsIniciado VALUE '00'.
			   88 UsTerminado VALUE '10'.
	   
	   PROCEDURE DIVISION.
		   OPEN INPUT UsuariosFile 
		   OPEN INPUT ArticulosFile
		   OPEN INPUT AlmacenesFile
           OPEN OUTPUT ReportFile
		   
		   WRITE PrintLine FROM Cabecera1 BEFORE ADVANCING 2 LINES
		   WRITE PrintLine FROM Cabecera2 BEFORE ADVANCING 1 LINES
		  
		   READ UsuariosFile 
           AT END SET EndOfUsersFile TO TRUE 
           END-READ  
		   
		   PERFORM UNTIL EndOfUsersFile
		   
		      READ ArticulosFile 
              AT END SET EndOfArticulosFile TO TRUE 
              END-READ	             			 
			 
			  PERFORM UNTIL EndOfArticulosFile			 
			 
			      IF UserId = UserArtID
			 
			         READ AlmacenesFile 
                     AT END SET EndOfAlmacenesFile TO TRUE
                     END-READ		
               		   
			        PERFORM UNTIL EndOfAlmacenesFile  
			   
			           
			            IF ArtID = ArtAlID
						    
							COMPUTE VariablePrecios = Cant * Precio
							MOVE UserId TO WUserId
							MOVE Name to WName
							MOVE Surname to WSurname
							MOVE Direccion to WDireccion
							MOVE Telefono to WTelefono
							MOVE ArtID TO WArtID
							MOVE Cant TO WCant
							MOVE Precio TO WPrecio
							MOVE VariablePrecios TO WTotal
							WRITE PrintLine FROM Todo
							
						END-IF
			   
				
						READ AlmacenesFile 
						AT END SET EndOfAlmacenesFile TO TRUE
										 
						END-READ
					
					END-PERFORM
					CLOSE AlmacenesFile
					OPEN INPUT AlmacenesFile
				END-IF
			 
					READ ArticulosFile 
					AT END SET EndOfArticulosFile TO TRUE
					
					END-READ 
             
   		       END-PERFORM	
		       CLOSE ArticulosFile
			   OPEN INPUT ArticulosFile		   
           READ UsuariosFile 
           AT END SET EndOfUsersFile TO TRUE 
           END-READ 
		   
           END-PERFORM


           CLOSE UsuariosFile, ArticulosFile, AlmacenesFile, ReportFile.
           STOP RUN.   	
	   
	   