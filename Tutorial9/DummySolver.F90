!------------------------------------------------------------------
! This is a dummy solver just to see if data can be extracted
!------------------------------------------------------------------

SUBROUTINE PreciceLoop(Model, Solver,dt,TransientSimulation)

!-------------------------------------------------------------------------
    USE DefUtils

    IMPLICIT NONE

!-------------------------------------------------------------------------
!Precice required variables
    CHARACTER*512                   :: config
    CHARACTER*50                    :: participantName, meshName, writeInitialData, readItCheckp, writeItCheckp
    INTEGER                         :: rank, commsize, ongoing, dimensions, meshID, vertexID, bool
    REAL                            :: dtlimit
    REAL, DIMENSION(:), ALLOCATABLE :: vertex 
    LOGICAL                         :: FirstTime
    
    SAVE FirstTime



!-------------------------------------------------------------------------
! Initialzation
    TYPE(Solver_t) :: Solver
    TYPE(Model_t)  :: Model
    REAL(KIND=dp)  :: dt
    LOGICAL        :: TransientSimulation
!-------------------------------------------------------------------------
! Local Variables
!-------------------------------------------------------------------------
    TYPE(Element_t),   POINTER :: Element
    TYPE(Element_t),   POINTER :: BoundaryElement
    TYPE(ValueList_t), POINTER :: BC
    REAL(KIND=dp), ALLOCATABLE :: localPressure(:)
    REAL(KIND=dp) :: Norm
    INTEGER :: n, t
    INTEGER :: iter, maxiter, bc_ID
    LOGICAL :: Found
    CHARACTER(LEN=MAX_NAME_LEN) :: bc_Name 
!-------------------------------------------------------------------------
! Local Variables
!-------------------------------------------------------------------------    
    ALLOCATE(localPressure(CurrentModel % MaxElementNodes))


!-------------------------------------------------------------------------
!Precice Initialization
!-------------------------------------------------------------------------
    IF (FirstTime) THEN    
        FirstTime = .FALSE.
    END IF

    ! Constants in f90 have to be prefilled with blanks to be compatible with preCICE
    writeInitialData(1:50)='                                                  '
    readItCheckp(1:50)='                                                  '
    writeItCheckp(1:50)='                                                  '
    
    CALL precicef_action_write_initial_data(writeInitialData)
    CALL precicef_action_read_iter_checkp(readItCheckp)
    CALL precicef_action_write_iter_checkp(writeItCheckp)
!-------------------------------------------------------------------------

!-------------------------------------------------------------------------
! Loop over Boundary Elements
!-------------------------------------------------------------------------
    DO t=1, CurrentModel % NumberOfBoundaryElements
        BoundaryElement => GetBoundaryElement(t)
        BC => GetBC()
        bc_ID = GetBCId( BoundaryElement )
        bc_Name = GetString(BC,'Name',Found)

        IF(bc_Name == 'inlet') THEN
            CALL GetScalarLocalSolution(localPressure, 'Pressure')
            ! CALL PrintPressure(BoundaryElement,localPressure)      
        END IF
    END DO


CONTAINS    

!-------------------------------------------------------------------------
! Helper functions
!-------------------------------------------------------------------------    
    SUBROUTINE PrintPressure(BoundaryElement,localPressure)
        TYPE(Element_t),   POINTER :: BoundaryElement
        REAL(KIND=dp) :: localPressure(:)
        INTEGER :: t,n

        n = GetElementNOFNodes(BoundaryElement)

        DO t=1,n
            ! WRITE(Message,'(A,I6,A,I6,A,ES12.3)') 'Element Index : ',BoundaryElement % ElementIndex,' Node Index: ' ,BoundaryElement % NodeIndexes(t), " Pressure Value: ", localPressure(t)
            WRITE(Message,'(A,I5,A,I5,A,ES12.3)') 'Element Index : ',BoundaryElement % ElementIndex &
                         ," Node Index: ", BoundaryElement % NodeIndexes(t) & 
                         , " Pressure Value: ", localPressure(t)
            CALL INFO('-----------------Precice----------------: ',Message) 
        END DO
    END SUBROUTINE PrintPressure

END SUBROUTINE PreciceLoop