!--------------------------------------------------------------------------------
!  Setting Coupling Boundary Values
!  From Dummy Solver
!--------------------------------------------------------------------------------


Function setTemp(Model,n,coordinate) Result(temperature)

    USE DefUtils

    IMPLICIT None

    TYPE(Model_t) :: Model
    INTEGER       :: n,dataID,valueIndex
    REAL(KIND=dp) :: coordinate,temperature

    TYPE(Variable_t), POINTER :: TimeVar
    Real(KIND=dp) :: Time

    CHARACTER*512                   :: config
    CHARACTER*50                    :: participantName, meshName, writeInitialData, readItCheckp, writeItCheckp
    CHARACTER*50                    :: readDataName, writeDataName
    INTEGER                         :: rank, commsize, ongoing, dimensions, meshID, bool, numberOfVertices, i,j
    INTEGER                         :: readDataID, writeDataID
    DOUBLE PRECISION                :: dt
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: vertices, writeData, readData
    INTEGER, DIMENSION(:), ALLOCATABLE :: vertexIDs


    writeInitialData(1:50)='                                                  '
    readItCheckp(1:50)='                                                  '
    writeItCheckp(1:50)='                                                  '

    CALL precicef_action_write_initial_data(writeInitialData)
    
    
    dataID = n
     valueIndex = mapCoordinateToID(coordinate)

    

    temperature =  300
    WRITE(Message, '(ES12.5)') temperature
    CALL INFO('-----------------temperature = ----------------: ',Message)
    

END FUNCTION setTemp

Function mapCoordinateToID(coordinate) 
    REAL(KIND=dp) :: coordinate
    INTEGER       :: mesh_id
    precice_interface = precice.Interface("HeatSolver", "precice-config.xml",0,1)
    mesh_id = precice_interface.get_mesh_id("TemperatureMesh")

END FUNCTION mapCoordinateToID