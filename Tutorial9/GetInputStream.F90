!-------------------------------
!  Inlet Boundary condition v
!  velocity in x-direction
!------------------------------


Function GetInputStream(model,n, y_coordinate) RESULT(velocity)

    USE DefUtils

    IMPLICIT None

    TYPE(Model_t) :: model
    INTEGER       :: n
    REAL(KIND=dp) :: y_coordinate,velocity
    
    INTEGER i
    REAL(KIND=dp), ALLOCATABLE :: pressure(:)
    ALLOCATE(pressure(CurrentModel % MaxElementNodes))

    CALL GetScalarLocalSolution(pressure,'Pressure')

   
      do 10 i = 1, CurrentModel % MaxElementNodes
        WRITE( Message,'(A,I10,A,ES12.3)') 'node:',i,' Pressure:',pressure(i)
        CALL Info('Pressure Data',Message)
    10 continue

    WRITE( Message,'(A,I10,A,I10)') 'node:',n,' Element:',CurrentModel % CurrentElement % ElementIndex
    CALL Info('Node Data',Message)

    velocity = 6*(y_coordinate-1)*(2-y_coordinate)

END FUNCTION GetInputStream