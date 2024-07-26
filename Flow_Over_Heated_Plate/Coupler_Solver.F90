MODULE HelperMethods

    !------------------------------------------------------------------------------
    USE DefUtils
    !------------------------------------------------------------------------------
    IMPLICIT NONE

    

    CONTAINS

    ! SUBROUTINE StoreCheckpoint(writeData,t)

    ! END SUBROUTINE StoreCheckpoint

    SUBROUTINE Print(dataName,mesh,BoundaryPerm,CoordVals)

        !-------------------------Strings----------------------------------------------
        CHARACTER(LEN=MAX_NAME_LEN)         :: dataName
        CHARACTER(LEN=MAX_NAME_LEN)         :: infoMessage
        !-------------------------Elmer_Types----------------------------------------------
        TYPE(Variable_t), POINTER           :: dataVariable
        TYPE(Mesh_t), POINTER               :: mesh
        !------------------------Data Arrays----------------------------------------------
        REAL(KIND=dp), POINTER              :: CoordVals(:)
        INTEGER, POINTER                    :: BoundaryPerm(:)
        !--------------------------Iterators-------------------------------------
        INTEGER                             :: i,j
        !--------------------------Mesh-------------------------------------
        INTEGER                         :: meshDim
        dataVariable  => VariableGet( mesh % Variables, dataName)
        meshDim = mesh % MaxDim
        CALL Info('CouplerSolver','Printing ' //TRIM(dataName))
        DO i = 1, mesh % NumberOfNodes
            j = BoundaryPerm(i)
            IF(j == 0) CYCLE
            IF (meshDim == 2) THEN
                write(infoMessage,'(A,I5,A,I5,A,F10.4,A,F10.2,A,F10.2)') 'Node: ',i,' Index: ',j,' Value: ' &
                            ,dataVariable % Values(dataVariable % Perm(i)),&
                            ' X= ', CoordVals(meshDim * j-1), ' Y= ', CoordVals(meshDim * j) 
            ELSE IF (meshDim == 3) THEN
                write(infoMessage,'(A,I5,A,I5,A,F10.4,A,F10.2,A,F10.2,A,F10.2)') 'Node: ',i,' Index: ',j,' Value: ' &
                            ,dataVariable % Values(dataVariable % Perm(i)),&
                            ' X= ', CoordVals(meshDim * j-2), ' Y= ', CoordVals(meshDim *j-1), ' Z= ', CoordVals(meshDim *j) 
            END IF                                        
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
        INTEGER, POINTER                    :: BoundaryPerm(:)
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

    SUBROUTINE CreateVariable(dataName,dataType,mesh,BoundaryPerm,Solver,solverParams)
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
        INTEGER, POINTER                    :: BoundaryPerm(:)
        !------------------------Mesh Data----------------------------------------------
        INTEGER                         :: Dofs
        !--------------------------Logic-Control-------------------------------------
        LOGICAL                             :: Found

        
        dataName =  ListGetString(solverParams,dataType,Found )
        dataVariable  => VariableGet( mesh % Variables, dataName)
        IF(ASSOCIATED( dataVariable ) ) THEN
            CALL Info('CouplerSolver','Using existing variable : '//TRIM(dataName) )
        ELSE
            CALL FATAL('CouplerSolver', 'Variable does not exist : ' // TRIM(dataName) )
            ! Dofs = ListGetInteger( solverParams,'Field Dofs',Found )
            ! IF(.NOT. Found ) Dofs = 1
            ! CALL VariableAddVector( mesh % Variables, mesh, Solver, dataName, Dofs, &
            !     Perm = BoundaryPerm, Secondary = .TRUE. )
            ! dataVariable => VariableGet( mesh % Variables, dataName ) 
        END IF


    END SUBROUTINE CreateVariable
    
    

    SUBROUTINE CopyReadData(dataName,mesh,BoundaryPerm,copyData)
        !-------------------------Strings-----------------------------------------------
        CHARACTER(LEN=MAX_NAME_LEN)         :: dataName
        !-------------------------Elmer_Types----------------------------------------------
        TYPE(Variable_t), POINTER           :: dataVariable
        TYPE(Mesh_t), POINTER               :: mesh
        ! !------------------------Data Arrays----------------------------------------------
        REAL(KIND=dp), POINTER              :: copyData(:)
        INTEGER, POINTER                    :: BoundaryPerm(:)
        !--------------------------Iterators-------------------------------------
        INTEGER                             :: i,j


        dataVariable  => VariableGet( mesh % Variables, dataName)

        DO i = 1, mesh % NumberOfNodes
            j = BoundaryPerm(i)
            IF(j == 0) CYCLE
            dataVariable % Values(dataVariable % Perm(i)) = copyData(j)
            

        END DO 

    END SUBROUTINE CopyReadData

    SUBROUTINE CopyWriteData(dataName,mesh,BoundaryPerm,copyData)
        !-------------------------Strings-----------------------------------------------
        CHARACTER(LEN=MAX_NAME_LEN)         :: dataName
        !-------------------------Elmer_Types----------------------------------------------
        TYPE(Variable_t), POINTER           :: dataVariable
        TYPE(Mesh_t), POINTER               :: mesh
        !------------------------Data Arrays----------------------------------------------
        REAL(KIND=dp), POINTER              :: copyData(:)
        INTEGER, POINTER                    :: BoundaryPerm(:)
        !--------------------------Iterators-------------------------------------
        INTEGER                             :: i,j


        dataVariable  => VariableGet( mesh % Variables, dataName)

        DO i = 1, mesh % NumberOfNodes
            j = BoundaryPerm(i)
            IF(j == 0) CYCLE
            copyData(j) = dataVariable % Values(dataVariable % Perm(i))
            ! IF( dataName == "temperature loads") THEN
            !     copyData(j) = -1 * dataVariable % Values(dataVariable % Perm(i)) 
            ! ELSE
            !     copyData(j) = dataVariable % Values(dataVariable % Perm(i))
            ! END IF
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
    !--------------------------Logic-Control-------------------------------------
    LOGICAL                             :: Found
    !--------------------------MPI-Variables-------------------------------------
    INTEGER                         :: rank,commsize
   
    !--------------------------Elmer-Variables-------------------------------------
    !-------------------------Loop_Control-------------------------------------
    INTEGER                         :: itask = 1
    !-------------------------Strings----------------------------------------------
    CHARACTER(LEN=MAX_NAME_LEN)         :: BoundaryName
    CHARACTER(LEN=MAX_NAME_LEN)         :: infoMessage
    !-------------------------Elmer_Types----------------------------------------------
    TYPE(Variable_t), POINTER           :: readDataVariable,writeDataVariable
    TYPE(Mesh_t), POINTER               :: mesh
    TYPE(ValueList_t), POINTER          :: simulation, solverParams ! Simulation gets Simulation list, & solverParams hold solver1,solver 2,etc
    !------------------------Data Arrays----------------------------------------------
    REAL(KIND=dp), POINTER              :: CoordVals(:)
    INTEGER, POINTER                    :: BoundaryPerm(:)
    !------------------------Time Variable----------------------------------------------
    TYPE(Variable_t), POINTER :: TimeVar
    Real(KIND=dp) :: Time

    !--------------------------preCICE-Variables-------------------------------------
    !-------------------------Strings----------------------------------------------
    CHARACTER(LEN=MAX_NAME_LEN)         :: config
    CHARACTER(LEN=MAX_NAME_LEN)         :: participantName, meshName
    CHARACTER(LEN=MAX_NAME_LEN)         :: readDataName, writeDataName

    !-------------------------IDs-Integer----------------------------------------------
    INTEGER                         :: meshID,readDataID, writeDataID, meshDim
    !------------------------Data Arrays----------------------------------------------
    REAL(KIND=dp), POINTER :: vertices(:), writeData(:), readData(:)
    INTEGER, POINTER                :: vertexIDs(:)
    !------------------------Mesh Data----------------------------------------------
    INTEGER                         :: BoundaryNodes
    INTEGER                         :: Dofs
    INTEGER                         :: dimensions ! ?? Do not know 
    !----------------------Time Loop Control Variables-----------------------------
    INTEGER                         :: bool
    INTEGER                         :: ongoing
    INTEGER                         :: i,j
    !--------------------------Variables-End-------------------------------------------

    !--------------------------SAVE-Start-------------------------------------------
    SAVE meshName,readDataName,writeDataName
    SAVE itask
    SAVE BoundaryPerm,CoordVals,vertexIDs
    SAVE readData,writeData
    SAVE BoundaryNodes
    !--------------------------SAVE-End-------------------------------------------


    !--------------------------Initialize-Start-------------------------------------------
    Simulation => GetSimulation()
    Mesh => Solver % Mesh
    solverParams => GetSolverParams()
    meshDim = Mesh % MaxDim
    
    rank = 0
    commsize = 1
    select case(itask)
    case(1)
        CALL Info('CouplerSolver ', 'Initializing Coupler Solver')
        !--- First Time Visited, Initialization
        !-- First Time Visit, Create preCICE, create nodes at interface
        !----------------------------- Initialize---------------------
        
        !----------------Acquire Names for solver---------------------
        BoundaryName = GetString( Simulation, 'maskName', Found )
        participantName = GetString( Simulation, 'participantName', Found )
        
        !-----------------Convert to preCICE Naming Convention    
        IF (participantName == 'solid') THEN
            participantName = 'Solid'
        END IF
        IF (participantName == 'fluid') THEN
            participantName = 'Fluid'
        END IF
        meshName = GetString( Simulation, 'meshName', Found )
        IF (meshName == 'solid-mesh') THEN
            meshName = 'Solid-Mesh'
        END IF
        IF (meshName == 'fluid-mesh') THEN
            meshName = 'Fluid-Mesh'
        END IF
        !---------------------------------------------------------------------

        !-----------------Get Config Path-------------------------------------
        config = GetString( Simulation, 'configPath', Found )

        Print *, TRIM(BoundaryName)," ",TRIM(participantName)," ",TRIM(meshName)," ",TRIM(config)
        
        !-----------Identify Vertex on Coupling Interface & Save Coordinates--------------------
        NULLIFY( BoundaryPerm )    
        ALLOCATE( BoundaryPerm( Mesh % NumberOfNodes ) )
        BoundaryPerm = 0
        BoundaryNodes = 0
        CALL MakePermUsingMask( Model,Model%Solver,Model%Mesh,BoundaryName,.FALSE., &
            BoundaryPerm,BoundaryNodes)
            
        CALL Info('CouplerSolver','Number of nodes at interface:'//TRIM(I2S(BoundaryNodes)))
        ALLOCATE( CoordVals(meshDim * BoundaryNodes) )
        ALLOCATE(vertexIDs(BoundaryNodes)) 
        DO i=1,Mesh % NumberOfNodes
            j = BoundaryPerm(i)
            IF(j == 0) CYCLE
            IF (meshDim == 2) THEN
                CoordVals(meshDim *j-1) = mesh % Nodes % x(i)
                CoordVals(meshDim*j)    = mesh % Nodes % y(i)
            ELSE IF (meshDim == 3) THEN
                CoordVals(meshDim *j-2) = mesh % Nodes % x(i)
                CoordVals(meshDim *j-1) = mesh % Nodes % y(i)
                CoordVals(meshDim *j)   = mesh % Nodes % z(i)                
            END IF            
        END DO
        
        CALL Info('CouplerSolver','Created nodes at interface')  

        ! !-----------Identify read and write Variables and Create it if it does not exist--------------------
        CALL CreateVariable(readDataName,'readDataName',mesh,BoundaryPerm,Solver,solverParams)
        CALL CreateVariable(writeDataName,'writeDataName',mesh,BoundaryPerm,Solver,solverParams)
        !-----------------------------------------------------------------------------------------

        ! !---------------Initializing preCICE------------------------------------------ 
        CALL Info('CouplerSolver','Initializing preCICE')     
        CALL precicef_create(participantName, config, rank, commsize)
        
        CALL Info('CouplerSolver','Setting up mesh in preCICE')
        CALL precicef_get_mesh_dimensions(meshName, dimensions)
        CALL precicef_set_vertices(meshName, BoundaryNodes, CoordVals, vertexIDs)
        ALLOCATE(readData(BoundaryNodes*dimensions))
        ALLOCATE(writeData(BoundaryNodes*dimensions))

        readData = 0
        writeData = 0

        CALL precicef_requires_initial_data(bool)
        IF (bool.EQ.1) THEN
            WRITE (*,*) 'TODO: Provide initial data if needed'
            CALL CopyWriteData(writeDataName,mesh,BoundaryPerm,writeData)
            IF (writeDataName == 'temperature') THEN
                CALL precicef_write_data(meshName, 'Temperature', BoundaryNodes, vertexIDs, writeData)
            ELSE IF (writeDataName == 'temperature flux_abs') THEN
                CALL precicef_write_data(meshName, 'Heat-Flux', BoundaryNodes, vertexIDs, writeData)
            ELSE
                CALL precicef_write_data(meshName, writeDataName, BoundaryNodes, vertexIDs, writeData)
            END IF
        ELSE
            WRITE (*,*) 'No initial data required'
        ENDIF
        CALL precicef_initialize()
        CALL precicef_is_coupling_ongoing(ongoing)
        ! !------------------------------------------------------------------------------

        itask = 2
    case(2)
        !-------------------Copy Read values from Variable to buffer---------------------
        CALL precicef_requires_reading_checkpoint(bool)

        IF (bool.EQ.1) THEN
            WRITE (*,*) 'DUMMY: Reading iteration checkpoint'
        ELSE
            WRITE (*,*) 'No reading iteration checkpoint required'
        ENDIF
        
        CALL Info('CouplerSolver ', 'Readinging the data from preCICE')    
        CALL precicef_get_max_time_step_size(dt)
        
        !-------------------Sticking preCICE Naming Convention-------------------------------------
        IF (readDataName == 'temperature') THEN
            CALL precicef_read_data(meshName, 'Temperature', BoundaryNodes, vertexIDs, dt, readData)
        ELSE IF (readDataName == 'temperature flux_abs') THEN
            CALL precicef_read_data(meshName, 'Heat-Flux', BoundaryNodes, vertexIDs, dt, readData)
        ELSE
            CALL precicef_read_data(meshName, readDataName, BoundaryNodes, vertexIDs, dt, readData)
        END IF

        CALL CopyReadData(readDataName,mesh,BoundaryPerm,readData)
        !-----------------------------------------------------------------------------------------

        itask = 3
    case(3)

        !-------------------Copy Write values from Variable to buffer----------------------------
        CALL CopyWriteData(writeDataName,mesh,BoundaryPerm,writeData)

        !-------------------Sticking preCICE Naming Convention-------------------------------------
        IF (writeDataName == 'temperature') THEN
            CALL precicef_write_data(meshName, 'Temperature', BoundaryNodes, vertexIDs, writeData)
        ELSE IF (writeDataName == 'temperature flux_abs') THEN
            CALL precicef_write_data(meshName, 'Heat-Flux', BoundaryNodes, vertexIDs, writeData)
        ELSE
            CALL precicef_write_data(meshName, writeDataName, BoundaryNodes, vertexIDs, writeData)
        END IF

        
        !-------------------Advance preCICE-------------------------------------------------------
        CALL precicef_advance(dt)
        CALL precicef_is_coupling_ongoing(ongoing)
        
        IF(ongoing.EQ.0) THEN
            itask = 4
        ELSE
            itask = 2
        END IF
        !-----------------------------------------------------------------------------------------

    case(4)
        CALL Info('CouplerSolver','preCICE Finalize')
        CALL precicef_finalize()

        ! DEALLOCATE(writeData)
        ! DEALLOCATE(readData)
        ! DEALLOCATE(vertexIDs)
    case(5)
        CALL Info('CouplerSolver ', 'Testing')
    end select

    CALL Info('CouplerSolver','Ended')
END SUBROUTINE CouplerSolver








