!------------------------------------------------------------------
! This is a dummy solver just to see if data can be extracted
!------------------------------------------------------------------

SUBROUTINE PreciceLoop(Model, Solver,dt,TransientSimulation)

!-------------------------------------------------------------------------
    USE DefUtils

    IMPLICIT NONE

!Notes for forum---------------------Step1:Defining local variable    
!-------------------------------------------------------------------------
! Initialzation
    TYPE(Solver_t) :: Solver
    TYPE(Model_t)  :: Model
    REAL(KIND=dp)  :: dt
    LOGICAL        :: TransientSimulation
!-------------------------------------------------------------------------
! Local Variables
!-------------------------------------------------------------------------
    TYPE(Element_t),   POINTER :: BoundaryElement
    TYPE(ValueList_t), POINTER :: BC
    REAL(KIND=dp), ALLOCATABLE :: localTemperature(:)
    REAL(KIND=dp), ALLOCATABLE :: localDummy(:)
    ! REAL(KIND=dp) :: localValues(:)
    INTEGER :: n, t
    INTEGER :: bc_ID
    LOGICAL :: Found
    CHARACTER(LEN=MAX_NAME_LEN) :: bc_Name, VariableName 
    REAL(KIND=dp), POINTER ::  localValues(:)
    REAL(KIND=dp) :: Norm

    CHARACTER*512                   :: config
    CHARACTER*50                    :: participantName, meshName, writeInitialData, readItCheckp, writeItCheckp
    CHARACTER*50                    :: readDataName, writeDataName
    INTEGER                         :: rank, commsize, ongoing, dimensions, meshID, bool, numberOfVertices, i,j
    INTEGER                         :: readDataID, writeDataID
    
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: vertices, writeData, readData
    INTEGER, DIMENSION(:), ALLOCATABLE :: vertexIDs


    writeInitialData(1:50)='                                                  '
    readItCheckp(1:50)='                                                  '
    writeItCheckp(1:50)='                                                  '

    CALL precicef_action_write_initial_data(writeInitialData)

!Notes for forum---------------------Step2:Allocating memory for retrieved data from Elmer     
!-------------------------------------------------------------------------
! Local Variables
!-------------------------------------------------------------------------    
    ALLOCATE(localTemperature(CurrentModel % MaxElementNodes))
    ALLOCATE(localDummy(CurrentModel % MaxElementNodes))
    ALLOCATE(localValues(CurrentModel % MaxElementNodes))

    VariableName = Model % Solvers(3) % Variable % Name
    localValues => Model % Solvers(3) % Variable % Values

    CALL DefaultDirichletBCs()
    
!Notes for forum---------------------Step3:looping over boundary elements    
!-------------------------------------------------------------------------
! Loop over Boundary Elements
!-------------------------------------------------------------------------
    DO t=1, CurrentModel % NumberOfBoundaryElements
        BoundaryElement => GetBoundaryElement(t)
        BC => GetBC()
        bc_ID = GetBCId( BoundaryElement )
        bc_Name = GetString(BC,'Name',Found)

        IF(bc_Name == 'coupling_boundary') THEN
            CALL GetScalarLocalSolution(localTemperature, 'Temperature')
            CALL PrintTemperature(BoundaryElement,localTemperature)      
        END IF
    END DO

    DO t=1, CurrentModel % NumberOfBoundaryElements
        BoundaryElement => GetBoundaryElement(t)
        BC => GetBC()
        bc_ID = GetBCId( BoundaryElement )
        bc_Name = GetString(BC,'Name',Found)

        IF(bc_Name == 'coupling_boundary') THEN
            CALL GetScalarLocalSolution(localDummy, 'DummyVariable')
            CALL PrintDummy(BoundaryElement,localDummy)      
        END IF
    END DO

    

CONTAINS    

!Notes for forum---------------------Step4:printing data of temperature and dummy variable 
!-------------------------------------------------------------------------
! Helper functions
!-------------------------------------------------------------------------    
    SUBROUTINE PrintTemperature(BoundaryElement,localTemperature)
        TYPE(Element_t),   POINTER :: BoundaryElement
        REAL(KIND=dp) :: localTemperature(:)
        INTEGER :: t,n

        n = GetElementNOFNodes(BoundaryElement)

        DO t=1,n
            WRITE(Message,'(A,I5,A,I5,A,ES12.5)') 'Element Index : ',BoundaryElement % ElementIndex &
                         ," Node Index: ", BoundaryElement % NodeIndexes(t) & 
                         , " Temperature Value: ", localTemperature(t)
            CALL INFO('-----------------Precice----------------: ',Message) 
        END DO
    END SUBROUTINE PrintTemperature

    SUBROUTINE PrintDummy(BoundaryElement,localDummy)
        TYPE(Element_t),   POINTER :: BoundaryElement
        REAL(KIND=dp) :: localDummy(:)
        INTEGER :: t,n

        n = GetElementNOFNodes(BoundaryElement)

        DO t=1,n
            WRITE(Message,'(A,I5,A,I5,A,ES12.5)') 'Element Index : ',BoundaryElement % ElementIndex &
                         ," Node Index: ", BoundaryElement % NodeIndexes(t) & 
                         , " Dummy Value: ", localDummy(t)
            CALL INFO('-----------------Precice----------------: ',Message) 
        END DO
    END SUBROUTINE PrintDummy

!-------------------------------------------------------------------------
! Commented Code
!------------------------------------------------------------------------- 

! VariableName = BoundaryElement % PropertyData % Name
! localValues => BoundaryElement % PropertyData % Values
! p => BoundaryElement % PropertyData
! IF(.not.associated(p)) THEN
!     WRITE(Message,'(A)') 'local Value: '         
!     CALL INFO('-----------------Precice----------------: ',Message) 
! ENDIF

! Do t=1, CurrentModel % NumberOfBoundaryElements
        
!     WRITE(Message,'(A,I3,A,ES12.5,A)') 'Node: ',t,'Value: ', localValues(t), VariableName     
!     CALL INFO('-----------------Precice----------------: ',Message) 
! END DO  

END SUBROUTINE PreciceLoop

! Hisham 26.1.21 Elmer


! # Hard-coded

! spec.sif
! ```
! BOUNDARY_CONDITION 1
!     ...
!     velocity 1 = 1.0
! ```

! # UDF

! spec.sif
! ```
! BOUNDARY_CONDITION 1
!     ...
!     velocity 1 = Variable Coordinate 2
!                 Real Procedure "myUDF_exe" "myUDF_function_name"
! ```

! udf.f90
! ```
! Function myUDF_function_name(Model, n, coordinate) Result(dataValue)
!     IMPLICIT None

!     TYPE(Model_t) :: Model
!     INTEGER       :: n, SolverId,dataValue
!     !return 1  !works
!     !return cordinate * 3  !works
!     !return my_dummy_precice_read(coordinate)
!     dataID = !comes from initialization
!     valueIndex = mapCoordinateToID(coordinate)
!     !from https://github.com/precice/precice/blob/6e8001562e2760c1548c73d88c40b1d939ca38b6/extras/bindings/fortran/include/precice/SolverInterfaceFortran.hpp#L639-L654
!     CALL precicef_read_sdata_(dataID, valueIndex, dataValue)
!     RETURN dataValue
!     END FUNCTION myUDF_function_name
! ```

! How to get `mapCoordinateToID`?

! python
! ```
! # during initialization

! precice_interface = precice.Interface("HeatSolver", "precice-config.xml",0,1)
! mesh_id = precice_interface.get_mesh_id("TemperatureMesh")
! mapCoordinateToID = {}
! for position in positions:
!     id = precice_interface.set_mesh_vertex(mesh_id, position)
!     mapCoordinateToID[position] = id
    
! data_id = precice_interface.get_data_id(mesh_id, "Temperature")

! print(mapCoordinateToID)
! # >> {(1,2,3):1, (3,4,5):2}
! # during simulation time

! value = compute_fancy_numbers_at(coordinate)
! vertex_id = mapCoordinateToID[coordinate]
! precice_interface.write_scalar_data(data_id, vertex_id, value)
! new_BC = precice_interface.read_scalar_data(data_id, vertex_id)
! mapCoordinateToID((2,3,4))
! # 1) >> ERROR!, 2) Interpolation? -> Worry about this later.
! ```

! In FEniCS
! ```
! random_coordinate  # from FEniCS
! value = compute_fancy_numbers_at(coordinate)
! vertex_id = mapCoordinateToID[coordinate]
! new_BC = precice_interface.read_scalar_data(data_id, vertex_id)
! ```
