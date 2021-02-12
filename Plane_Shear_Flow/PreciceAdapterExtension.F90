!-------------------------------
!  Setting Coupling Boundary Values
!  From Dummy Solver
!-------------------------------


Function PreciceAdapterExtension(Model,n,preciceVariable) Result(scalarVariable)

    USE DefUtils

    IMPLICIT None

    TYPE(Model_t) :: Model
    INTEGER       :: n, SolverId
    REAL(KIND=dp) :: preciceVariable,scalarVariable
    LOGICAL :: Visited = .FALSE.
    CHARACTER(LEN=MAX_NAME_LEN) :: solverName 

    solverId = Model  % Solver % SolverId

    


    ! IF(.NOT.Visited) THEN
    !     scalarVariable = 300
    !     return
    ! END IF
    
    ! IF(solverId /= 3) THEN
    !     return
    ! END IF

    scalarVariable = 700
    


    IF(SolverId == 2) THEN
        scalarVariable = 80 
        WRITE(Message, '(A)') "Temp = 80"
        CALL INFO('-----------------Solver 2----------------: ',Message)
    END IF

    IF(SolverId == 3) THEN
        scalarVariable = 100 
        WRITE(Message, '(A)') "Temp = 100"
        CALL INFO('-----------------Solver 3----------------: ',Message)
    END IF


    ! IF(SolverId == 3) THEN       
    !     WRITE(Message,'(A)')  "Hellllo"   
    !     CALL INFO('-----------------Precice----------------: ',Message)
    !     scalarVariable = 80 
    ! else if(SolverId == 2) then
    !     WRITE(Message,'(A)')  "Fuck"   
    !     CALL INFO('-----------------Precice----------------: ',Message)
    !     scalarVariable = 200
    ! END IF

    WRITE(Message,'(ES12.5,I3,I3)')  scalarVariable,solverId,n   
    CALL INFO('-----------------Precice----------------: ',Message) 

END FUNCTION PreciceAdapterExtension