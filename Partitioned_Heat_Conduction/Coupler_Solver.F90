MODULE HelperMethods

    !------------------------------------------------------------------------------
    USE DefUtils
    !------------------------------------------------------------------------------
    IMPLICIT NONE

    

    CONTAINS

    ! SUBROUTINE StoreCheckpoint(writeData,t)

    ! END SUBROUTINE StoreCheckpoint

    SUBROUTINE Print(dataName,mesh,BCPerm,CoordVals)

        !-------------------------Strings----------------------------------------------
        CHARACTER(LEN=MAX_NAME_LEN)         :: dataName
        CHARACTER(LEN=MAX_NAME_LEN)         :: infoMessage
        !-------------------------Elmer_Types----------------------------------------------
        TYPE(Variable_t), POINTER           :: dataVariable
        TYPE(Mesh_t), POINTER               :: mesh
        !------------------------Data Arrays----------------------------------------------
        REAL(KIND=dp), POINTER              :: CoordVals(:)
        INTEGER, POINTER                    :: BCPerm(:)
        !--------------------------Iterators-------------------------------------
        INTEGER                             :: i,j
    
        dataVariable  => VariableGet( mesh % Variables, dataName)

        CALL Info('CouplerSolver','Printing ' //TRIM(dataName))
        DO i = 1, mesh % NumberOfNodes
            j = BCPerm(i)
            IF(j == 0) CYCLE
            
            write(infoMessage,'(A,I5,A,I5,A,F10.4,A,F10.2,A,F10.2)') 'Node: ',i,' Index: ',j,' Value: ' &
                            ,dataVariable % Values(dataVariable % Perm(i)),&
                            ' X= ', CoordVals(2*j-1), ' Y= ', CoordVals(2*j)  
                                                          
            CALL Info('CouplerSolver',infoMessage)

        END DO 

    END SUBROUTINE Print

    SUBROUTINE PrintDomain(dataName,mesh)

        !-------------------------Strings----------------------------------------------
        ! CHARACTER(LEN=MAX_NAME_LEN)         :: dataName
        CHARACTER(LEN=MAX_NAME_LEN)         :: infoMessage
        character(len=*), intent(in) ::      dataName
        !-------------------------Elmer_Types----------------------------------------------
        TYPE(Variable_t), POINTER           :: dataVariable
        TYPE(Mesh_t), POINTER               :: mesh
        !------------------------Data Arrays----------------------------------------------
        REAL(KIND=dp), POINTER              :: CoordVals(:)
        INTEGER, POINTER                    :: BCPerm(:)
        !--------------------------Iterators-------------------------------------
        INTEGER                             :: i,j
    
        dataVariable  => VariableGet( mesh % Variables, dataName)

        CALL Info('CouplerSolver','Printing ' //TRIM(dataName))
        DO i = 1, mesh % NumberOfNodes
            
            
            write(infoMessage,'(A,I5,A,F10.4,A,F10.2,A,F10.2)') 'Node: ',i,' Value: ' &
                            ,dataVariable % Values(dataVariable % Perm(i)),&
                            ' X= ', mesh % Nodes % x(i), ' Y= ', mesh % Nodes % y(i) 
                                                          
            CALL Info('CouplerSolver',infoMessage)

        END DO 
    END SUBROUTINE PrintDomain

    SUBROUTINE CreateVariable(dataName,dataType,mesh,BCPerm,Solver,solverParams)
        !-------------------------Strings-----------------------------------------------
        CHARACTER(LEN=MAX_NAME_LEN)         :: dataName
        CHARACTER(LEN=MAX_NAME_LEN)         :: infoMessage
        character(len=*), intent(in) :: dataType
        !-------------------------Elmer_Types-------------------------------------------
        TYPE(Variable_t), POINTER           :: dataVariable
        TYPE(Mesh_t), POINTER               :: mesh
        TYPE(Solver_t)                      :: Solver
        TYPE(ValueList_t), POINTER          :: solverParams
        !------------------------Data Arrays--------------------------------------------
        INTEGER, POINTER                    :: BCPerm(:)
        !------------------------Mesh Data----------------------------------------------
        INTEGER                         :: Dofs
        !--------------------------Logic-Control-------------------------------------
        LOGICAL                             :: Found

        
        dataName =  ListGetString(solverParams,dataType,Found )
        dataVariable  => VariableGet( mesh % Variables, dataName)
        IF(ASSOCIATED( dataVariable ) ) THEN
            CALL Info('CouplerSolver','Using existing variable : '//TRIM(dataName) )
        ELSE
            CALL Info('CouplerSolver','Creating variable  as it does not exist: '//TRIM(dataName))

            Dofs = ListGetInteger( solverParams,'Field Dofs',Found )
            IF(.NOT. Found ) Dofs = 1
            CALL VariableAddVector( mesh % Variables, mesh, Solver, dataName, Dofs, &
                Perm = BCPerm, Secondary = .TRUE. )
            dataVariable => VariableGet( mesh % Variables, dataName ) 
        END IF


    END SUBROUTINE CreateVariable
    
    

    SUBROUTINE CopyReadData(dataName,mesh,BCPerm,copyData)
        !-------------------------Strings-----------------------------------------------
        CHARACTER(LEN=MAX_NAME_LEN)         :: dataName
        !-------------------------Elmer_Types----------------------------------------------
        TYPE(Variable_t), POINTER           :: dataVariable
        TYPE(Mesh_t), POINTER               :: mesh
        ! !------------------------Data Arrays----------------------------------------------
        REAL(KIND=dp), POINTER              :: copyData(:)
        INTEGER, POINTER                    :: BCPerm(:)
        !--------------------------Iterators-------------------------------------
        INTEGER                             :: i,j


        dataVariable  => VariableGet( mesh % Variables, dataName)

        DO i = 1, mesh % NumberOfNodes
            j = BCPerm(i)
            IF(j == 0) CYCLE
            dataVariable % Values(dataVariable % Perm(i)) = copyData(j)
            

        END DO 

    END SUBROUTINE CopyReadData

    SUBROUTINE CopyWriteData(dataName,mesh,BCPerm,copyData)
        !-------------------------Strings-----------------------------------------------
        CHARACTER(LEN=MAX_NAME_LEN)         :: dataName
        !-------------------------Elmer_Types----------------------------------------------
        TYPE(Variable_t), POINTER           :: dataVariable
        TYPE(Mesh_t), POINTER               :: mesh
        !------------------------Data Arrays----------------------------------------------
        REAL(KIND=dp), POINTER              :: copyData(:)
        INTEGER, POINTER                    :: BCPerm(:)
        !--------------------------Iterators-------------------------------------
        INTEGER                             :: i,j


        dataVariable  => VariableGet( mesh % Variables, dataName)

        DO i = 1, mesh % NumberOfNodes
            j = BCPerm(i)
            IF(j == 0) CYCLE
            ! USE THIS LINE FOR ELMER-ELMER
            copyData(j) = (dataVariable % Values(dataVariable % Perm(i)))

            ! USE THIS LINE FOR ELMER-FENICS
            !copyData(j) = -1 * dataVariable % Values(dataVariable % Perm(i)) 
        END DO 
    END SUBROUTINE CopyWriteData

END MODULE HelperMethods



SUBROUTINE CouplerSolver( Model,Solver,dt,TransientSimulation)

    !------------------------------------------------------------------------------
    USE DefUtils
    USE HelperMethods
    !------------------------------------------------------------------------------
    IMPLICIT NONE
    !--------------------------UDS Prerequistes------------------------------------
    TYPE(Solver_t) :: Solver
    TYPE(Model_t) :: Model
    REAL(KIND=dp) :: dt
    LOGICAL :: TransientSimulation


    !--------------------------Variables-Start-------------------------------------------
    
    !--------------------------MPI-Variables-------------------------------------
    INTEGER                         :: rank,commsize
    !--------------------------Precice-Control-------------------------------------
    INTEGER                         :: itask = 1

    !--------------------------Logic-Control-------------------------------------
    LOGICAL                             :: Found
    !--------------------------Iterators-------------------------------------
    INTEGER                             :: i,j


    !--------------------------Elmer-Variables-------------------------------------
    !-------------------------Strings----------------------------------------------
    CHARACTER(LEN=MAX_NAME_LEN)         :: maskName
    CHARACTER(LEN=MAX_NAME_LEN)         :: infoMessage
    !-------------------------Elmer_Types----------------------------------------------
    TYPE(Variable_t), POINTER           :: readDataVariable,writeDataVariable
    TYPE(Mesh_t), POINTER               :: mesh
    TYPE(ValueList_t), POINTER          :: simulation, solverParams ! Simulation gets Simulation list, & solverParams hold solver1,solver 2,etc
    !------------------------Data Arrays----------------------------------------------
    REAL(KIND=dp), POINTER              :: CoordVals(:)
    INTEGER, POINTER                    :: BCPerm(:)
    !------------------------Time Variable----------------------------------------------
    TYPE(Variable_t), POINTER :: TimeVar
    Real(KIND=dp) :: Time

    !--------------------------Precice-Variables-------------------------------------
    !-------------------------Strings----------------------------------------------
    CHARACTER(LEN=MAX_NAME_LEN)         :: participantName, meshName, configPath, readDataName,writeDataName
    ! CHARACTER(LEN=MAX_NAME_LEN)         :: writeInitialData, readItCheckp, writeItCheckp ! ?? Do not know
    CHARACTER*50                        :: writeInitialData, readItCheckp, writeItCheckp
    !-------------------------IDs-Integer----------------------------------------------
    INTEGER                         :: meshID,readDataID, writeDataID
    !------------------------Data Arrays----------------------------------------------
    REAL(KIND=dp), POINTER :: writeData(:), readData(:)
    INTEGER, POINTER                :: vertexIDs(:)
    !------------------------Mesh Data----------------------------------------------
    INTEGER                         :: vertexSize
    INTEGER                         :: Dofs
    INTEGER                         :: dimensions ! ?? Do not know 
    !----------------------Time Loop Control Variables-----------------------------
    INTEGER                         :: bool
    INTEGER                         :: ongoing
    !--------------------------Variables-End-------------------------------------------

    integer, dimension (11) :: vertecies

    !--------------------------SAVE-Start-------------------------------------------
    SAVE meshID,readDataID,writeDataID
    SAVE meshName,readDataName,writeDataName
    SAVE itask
    SAVE BCPerm,CoordVals,vertexIDs
    SAVE readData,writeData
    SAVE vertexSize

    !--------------------------SAVE-End-------------------------------------------

    !--------------------------Initialize-Start-------------------------------------------
    CALL Info(''//achar(27)//'[31mCouplerSolver ', 'Transfering results between different software ')

    Simulation => GetSimulation()
    Mesh => Solver % Mesh
    solverParams => GetSolverParams()

    rank = 0
    commsize = 1
    !--------------------------Initialize-End-------------------------------------------
    !----Dirichlet
    ! vertecies = (/3,22,21,20,19,18,17,16,15,14,4/)
    ! vertecies = (/2,32,33,34,35,36,37,38,39,40,1/)
    !----Neumann
    vertecies = (/4,32,33,34,35,36,37,38,39,40,1/)
    
    ! CALL Info('CouplerSolver','Enter Key To Continue')
    ! read(*,*)

    writeInitialData(1:50)='                                                  '
    readItCheckp(1:50)='                                                  '
    writeItCheckp(1:50)='                                                  '

    select case(itask)
        ! TODO make enum
    case(1)
        !--- First Time Visited, Initialization
        !-- First Time Visit, Create Precice, create nodes at interface
        !----------------------------- Initialize---------------------
        !----------------Acquire Names for solver---------------------
        maskName = GetString( Simulation, 'maskName', Found )
        participantName = GetString( Simulation, 'participantName', Found )
        meshName = GetString( Simulation, 'meshName', Found )
        configPath = GetString( Simulation, 'configPath', Found )

        Print *, TRIM(maskName)," ",TRIM(participantName)," ",TRIM(meshName)," ",TRIM(configPath)
        

        !-----------Identify Vertex on Coupling Interface & Save Coordinates--------------------
        NULLIFY( BCPerm )    
        ALLOCATE( BCPerm( Mesh % NumberOfNodes ) )
        BCPerm = 0
        ! CALL MakePermUsingMask( Model, Solver, Mesh, MaskName, .FALSE., &
        !     BCPerm, vertexSize )
        CALL MakePermUsingMask( Model, Solver, Mesh, MaskName, .TRUE., &
            BCPerm, vertexSize )
        CALL Info('CouplerSolver','Number of nodes at interface:'//TRIM(I2S(vertexSize)))

        

        ALLOCATE( CoordVals(2*vertexSize) )
        ALLOCATE(vertexIDs(vertexSize)) 
        DO i=1,Mesh % NumberOfNodes
            j = BCPerm(i)
            CoordVals(2*j-1) = mesh % Nodes % x(i)
            CoordVals(2*j) = mesh % Nodes % y(i)
            ! CoordVals(3*j) = mesh % Nodes % z(i)
            ! IF(j /= 0) THEN
            !     vertexIDs(j) = j
            ! END IF
        END DO
        CALL Info('CouplerSolver','Created nodes at interface')   
        
        !-----------Identify read Variables and Create it if it does not exist--------------------
        CALL CreateVariable(readDataName,'readDataName',mesh,BCPerm,Solver,solverParams)

        !-----------Print Read Values, For Debugging Purposes--------------------
        CALL Info('CouplerSolver','Printing read Data')
        CALL Print(readDataName,mesh ,BCPerm,CoordVals)
        !------------------------------------------------------------------------------
       
        !-----------Identify write Variables and Create it if it does not exist--------------------
        CALL CreateVariable(writeDataName,'writeDataName',mesh,BCPerm,Solver,solverParams)

        !-----------Print Write Values, For Debugging Purposes--------------------
        CALL Info('CouplerSolver','Printing write Data')
        CALL Print(writeDataName,mesh ,BCPerm,CoordVals)

        ! !------------------------------------------------------------------------------
        ! !---------------Initializing Precice------------------------------------------
        
        ! IF (participantName == "neumann") THEN
        !     rank =1
        ! END IF
        
        CALL precicef_create(participantName, configPath, rank, commsize)
        
        writeInitialData(1:50)='                                                  '
        readItCheckp(1:50)='                                                  '
        writeItCheckp(1:50)='                                                  '

        CALL precicef_get_dims(dimensions)
        CALL precicef_get_mesh_id(meshName, meshID)
        CALL precicef_set_vertices(meshID, vertexSize, CoordVals, vertexIDs)

        CALL precicef_get_data_id(readDataName,meshID,readDataID)
        CALL precicef_get_data_id(writeDataName,meshID,writeDataID)

        ALLOCATE(readData(VertexSize))
        ALLOCATE(writeData(VertexSize))

        readData = 0
        writeData = 0


        !----------------------Initializing Data------------------------------------
        CALL precicef_initialize(dt)
        CALL precicef_action_write_initial_data(writeInitialData)
        CALL precicef_is_action_required(writeInitialData, bool)
        
        
        
        IF (bool.EQ.1) THEN
            CALL Info('CouplerSolver','Writing Initial Data')
            CALL CopyWriteData(writeDataName,mesh,BCPerm,writeData)
            CALL precicef_write_bsdata(writeDataID, vertexSize, vertexIDs, writeData)
            CALL precicef_mark_action_fulfilled(writeInitialData)
        ENDIF

        CALL precicef_initialize_data()
      
        CALL precicef_is_coupling_ongoing(ongoing)
        

        itask = 2
    case(2)

        CALL precicef_action_write_iter_checkp(writeItCheckp)
        CALL precicef_is_action_required(writeItCheckp, bool)
        
        write(infoMessage,'(A,I2)') writeItCheckp,bool
        CALL Info('CouplerSolver',infoMessage)

        IF (bool.EQ.1) THEN
          CALL Info('CouplerSolver','Writing iteration checkpoint')
          CALL precicef_mark_action_fulfilled(writeItCheckp)
        ENDIF

        CALL Info('CouplerSolver','Reading Data')
        CALL precicef_read_bsdata(readDataID, vertexSize, vertexIDs, readData)

        CALL Info('CouplerSolver','Copy Read Data to Variable')
        CALL CopyReadData(readDataName,mesh,BCPerm,readData)

        CALL Info('CouplerSolver','Printing read Data')
        CALL Print(readDataName,mesh ,BCPerm,CoordVals)
        

        CALL Info('CouplerSolver','Printing write Data')
        CALL Print(writeDataName,mesh ,BCPerm,CoordVals)
        

        itask = 3
    case(3)
        !-------------------Copy Write values from Variable to buffer---------------------
        
        CALL Info('CouplerSolver','Copy Write Data to Variable')
        CALL CopyWriteData(writeDataName,mesh,BCPerm,writeData)

        CALL Info('CouplerSolver','Writing Data')
        CALL precicef_write_bsdata(writeDataID, vertexSize, vertexIDs, writeData)

        CALL Info('CouplerSolver','Printing write Data')
        CALL Print(writeDataName,mesh ,BCPerm,CoordVals)
        
        CALL Info('CouplerSolver','Printing read Data')
        CALL Print(readDataName,mesh ,BCPerm,CoordVals)

        !--------------------Advance time loop---------------------------------------
        CALL precicef_advance(dt)

        CALL precicef_action_read_iter_checkp(readItCheckp)
        CALL precicef_is_action_required(readItCheckp, bool)
        
        IF (bool.EQ.1) THEN
         
          write(infoMessage,'(A,I2)') readItCheckp,bool
          CALL Info('CouplerSolver',infoMessage)  
          CALL Info('CouplerSolver','Reading iteration checkpoint')
          CALL precicef_mark_action_fulfilled(readItCheckp)
        ENDIF

        

        CALL precicef_is_coupling_ongoing(ongoing)

        IF(ongoing.EQ.0) THEN
            itask = 4
        ELSE
            itask = 2
        END IF
          
    case(4)    
        !----------------------------------------Finailize--------------------------------------
        CALL Info('CouplerSolver','Precice Finalize')
        CALL precicef_finalize()

    case(5)
        ! CALL PrintDomain('temperature loads',mesh)
        CALL Info('CouplerSolver','Printing Temperature')
        readDataVariable  => VariableGet( mesh % Variables, 'temperature ')
        DO i = 1, 11
            
            
            write(infoMessage,'(A,I5,A,F10.4)') 'Node: ',vertecies(i),' Value: ', &
                        readDataVariable % Values(readDataVariable % Perm(vertecies(i)))
                          
            CALL Info('CouplerSolver',infoMessage)
            
        END DO

        CALL Info('CouplerSolver','Printing temperature loads')
        readDataVariable  => VariableGet( mesh % Variables, 'temperature loads')
        DO i = 1, 11
            
            
            write(infoMessage,'(A,I5,A,F10.4)') 'Node: ',vertecies(i),' Value: ', &
                        readDataVariable % Values(readDataVariable % Perm(vertecies(i)))
                          
            CALL Info('CouplerSolver',infoMessage)
            
        END DO
    end select


    CALL Info('CouplerSolver',' Ended '//achar(27)//'[0m.')
END SUBROUTINE CouplerSolver








