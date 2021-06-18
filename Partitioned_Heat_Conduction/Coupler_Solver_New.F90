SUBROUTINE CouplerSolver( Model,Solver,dt,TransientSimulation)
    !------------------------------------------------------------------------------
    USE DefUtils
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
    CHARACTER(LEN=MAX_NAME_LEN)         :: writeInitialData, readItCheckp, writeItCheckp ! ?? Do not know
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

    
    select case(itask)
        ! TODO make enum
    case(1)
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

        ALLOCATE( CoordVals(3*vertexSize) )
        ALLOCATE(vertexIDs(vertexSize)) 
        DO i=1,Mesh % NumberOfNodes
            j = BCPerm(i)
            CoordVals(3*j-2) = mesh % Nodes % x(i)
            CoordVals(3*j-1) = mesh % Nodes % y(i)
            CoordVals(3*j) = mesh % Nodes % z(i)
            IF(j /= 0) THEN
                vertexIDs(j) = j
            END IF
        END DO
        CALL Info('CouplerSolver','Created nodes at interface')   
        
        !-----------Identify read Variables and Create it if it does not exist--------------------
        readDataName =  ListGetString(solverParams,'readDataName',Found )
        readDataVariable  => VariableGet( mesh % Variables, readDataName)
        IF(ASSOCIATED( readDataVariable ) ) THEN
            CALL Info('CouplerSolver','Using existing variable for reading: '//TRIM(readDataName) )
        ELSE
            CALL Info('CouplerSolver','Creating variable for reading as it does not exist: '//TRIM(readDataName))

            Dofs = ListGetInteger( solverParams,'Field Dofs',Found )
            IF(.NOT. Found ) Dofs = 1
            CALL VariableAddVector( mesh % Variables, mesh, Solver, readDataName, Dofs, &
                Perm = BCPerm, Secondary = .TRUE. )
            readDataVariable => VariableGet( mesh % Variables, readDataName ) 
        END IF


        !-----------Print Read Values, For Debugging Purposes--------------------
        readDataVariable  => VariableGet( mesh % Variables, readDataName)
        CALL Info('CouplerSolver','Printing read Data')
        DO i = 1, mesh % NumberOfNodes
            j = BCPerm(i)
            IF(j == 0) CYCLE
            
            write(infoMessage,'(A,I5,A,I5,A,F10.4,A,F10.2,A,F10.2)') 'Node: ',i,' Index: ',j,' Value: ', &
                        readDataVariable % Values(readDataVariable % Perm(i)), &
                        ' X= ', CoordVals(3*j-2), ' Y= ', CoordVals(3*j-1)   
            CALL Info('CouplerSolver',infoMessage)
            
        END DO
        !------------------------------------------------------------------------------
       
        !-----------Identify write Variables and Create it if it does not exist--------------------
        writeDataName =  ListGetString(solverParams,'writeDataName',Found )
        writeDataVariable  => VariableGet( mesh % Variables, writeDataName)
        IF(ASSOCIATED( writeDataVariable ) ) THEN
            CALL Info('CouplerSolver','Using existing variable for writing: '//TRIM(writeDataName))
        ELSE    
            CALL Info('CouplerSolver','Creating variable for writing as it does not exist: '//TRIM(writeDataName))
            Dofs = ListGetInteger( solverParams,'Field Dofs',Found )
            IF(.NOT. Found ) Dofs = 1
            CALL VariableAddVector( mesh % Variables, mesh, Solver, writeDataName, Dofs, &
                Perm = BCPerm, Secondary = .TRUE. )
            writeDataVariable => VariableGet( mesh % Variables, writeDataName )
            
        END IF 

        !-----------Print Write Values, For Debugging Purposes--------------------
        writeDataVariable  => VariableGet( mesh % Variables, writeDataName)
        ! writeDataVariable % Values = 7
        CALL Info('CouplerSolver','Printing write Data')
        DO i = 1, mesh % NumberOfNodes
            j = BCPerm(i)
            IF(j == 0) CYCLE
            
            write(infoMessage,'(A,I5,A,I5,A,F10.4,A,F10.2,A,F10.2)') 'Node: ',i,' Index: ',j,' Value: ', &
                                writeDataVariable % Values(writeDataVariable % Perm(i)), &
                                ' X= ', CoordVals(3*j-2), ' Y= ', CoordVals(3*j-1)   
            CALL Info('CouplerSolver',infoMessage)
        END DO

        !------------------------------------------------------------------------------

        !---------------Initializing Precice------------------------------------------
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

        CALL precicef_initialize(dt)
        CALL precicef_is_action_required(writeInitialData, bool)

        IF (bool.EQ.1) THEN
            CALL Info('CouplerSolver','Writing Initial Data')
        ENDIF

        CALL precicef_initialize_data()
      
        CALL precicef_is_coupling_ongoing(ongoing)

        itask = 2

    case(2)
        !--------------------Reading read Data to preCICE---------------------------
        CALL precicef_read_bsdata(readDataID, vertexSize, vertexIDs, readData)  
        
        readDataVariable  => VariableGet( mesh % Variables, readDataName)
        CALL Info('CouplerSolver','Printing read Data')
        DO i = 1, mesh % NumberOfNodes
            j = BCPerm(i)
            IF(j == 0) CYCLE
            readDataVariable % Values(readDataVariable % Perm(i)) = readData(j)
            write(infoMessage,'(A,I5,A,I5,A,F10.4,A,F10.2,A,F10.2)') 'Node: ',i,' Index: ',j,' Value: ' &
                            ,readDataVariable % Values(readDataVariable % Perm(i)),&
                            ' X= ', CoordVals(3*j-2), ' Y= ', CoordVals(3*j-1)                                
            CALL Info('CouplerSolver',infoMessage)

        END DO 

        itask = 3
    case(3)
        !-------------------Copy Write values from Variable to buffer---------------------
        writeDataVariable  => VariableGet( mesh % Variables, writeDataName)
        CALL Info('CouplerSolver','Printing write Data')
        DO i = 1, mesh % NumberOfNodes
            j = BCPerm(i)
            IF(j == 0) CYCLE
            writeData(j) = writeDataVariable % Values(writeDataVariable % Perm(i)) 
            write(infoMessage,'(A,I5,A,I5,A,F10.4,A,F10.2,A,F10.2)') 'Node: ',i,' Index: ',j,' Value: ',&
                    writeDataVariable % Values(writeDataVariable % Perm(i)), &
                    ' X= ', CoordVals(3*j-2), ' Y= ', CoordVals(3*j-1)
            CALL Info('CouplerSolver',infoMessage)
            
        END DO

        !--------------------Writing write Data to preCICE---------------------------
        CALL precicef_write_bsdata(writeDataID, vertexSize, vertexIDs, writeData)


        !--------------------Advance time loop---------------------------------------
        CALL precicef_advance(dt)
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

    case(7)


        CALL precicef_is_action_required(writeItCheckp, bool)

        IF (bool.EQ.1) THEN
          CALL Info('CouplerSolver','Writing iteration checkpoint')
          CALL precicef_mark_action_fulfilled(writeItCheckp)
        ENDIF


        !--------------------Reading read Data to preCICE---------------------------
        CALL precicef_read_bsdata(readDataID, vertexSize, vertexIDs, readData)    

        readDataVariable  => VariableGet( mesh % Variables, readDataName)
        CALL Info('CouplerSolver','Printing read Data')
        DO i = 1, mesh % NumberOfNodes
            j = BCPerm(i)
            IF(j == 0) CYCLE
            readDataVariable % Values(readDataVariable % Perm(i)) = readData(j)
            write(infoMessage,'(A,I5,A,I5,A,F10.4,A,F10.2,A,F10.2)') 'Node: ',i,' Index: ',j,' Value: ' &
                            ,readDataVariable % Values(readDataVariable % Perm(i)),&
                            ' X= ', CoordVals(3*j-2), ' Y= ', CoordVals(3*j-1)                                
            CALL Info('CouplerSolver',infoMessage)

        END DO            

        ! TimeVar => VariableGet( Solver % Mesh % Variables, 'Time' )
        ! Time = TimeVar % Values(1)

        !-------------------Copy Write values from Variable to buffer---------------------
        writeDataVariable  => VariableGet( mesh % Variables, writeDataName)
        CALL Info('CouplerSolver','Printing write Data')
        DO i = 1, mesh % NumberOfNodes
            j = BCPerm(i)
            IF(j == 0) CYCLE
            writeData(j) = writeDataVariable % Values(writeDataVariable % Perm(i)) 
            write(infoMessage,'(A,I5,A,I5,A,F10.4,A,F10.2,A,F10.2)') 'Node: ',i,' Index: ',j,' Value: ',&
                    writeDataVariable % Values(writeDataVariable % Perm(i)), &
                    ' X= ', CoordVals(3*j-2), ' Y= ', CoordVals(3*j-1)
            CALL Info('CouplerSolver',infoMessage)
            
        END DO

        !--------------------Writing write Data to preCICE---------------------------
        CALL precicef_write_bsdata(writeDataID, vertexSize, vertexIDs, writeData)


        !--------------------Advance time loop---------------------------------------
        CALL precicef_advance(dt)
        CALL precicef_is_coupling_ongoing(ongoing)


        !-------------------IF Simulation Finished,Finalize--------------------------
        IF(ongoing.EQ.0) THEN
            CALL Info('CouplerSolver','Precice Finalize')
            CALL precicef_finalize()
        END IF
        
        
    case(8)

        maskName = GetString( Simulation, 'maskName', Found )

        NULLIFY( BCPerm )    
        ALLOCATE( BCPerm( Mesh % NumberOfNodes ) )
        BCPerm = 0
        ! CALL MakePermUsingMask( Model, Solver, Mesh, MaskName, .FALSE., &
        !     BCPerm, vertexSize )
        CALL MakePermUsingMask( Model, Solver, Mesh, MaskName, .TRUE., &
            BCPerm, vertexSize )
        
        readDataName =  ListGetString(solverParams,'readDataName',Found )
        readDataVariable  => VariableGet( mesh % Variables, readDataName)

        writeDataName =  ListGetString(solverParams,'writeDataName',Found )
        writeDataVariable  => VariableGet( mesh % Variables, writeDataName)
        
        ALLOCATE( CoordVals(3*vertexSize) )
        DO i=1,Mesh % NumberOfNodes
            j = BCPerm(i)
            CoordVals(3*j-2) = mesh % Nodes % x(i)
            CoordVals(3*j-1) = mesh % Nodes % y(i)
            CoordVals(3*j) = mesh % Nodes % z(i)
            ! readDataVariable % Values(readDataVariable % Perm(i)) = j
        END DO
        itask = 4
        
    case(9)
        CALL Info('CouplerSolver','Printing read Data')
        readDataVariable  => VariableGet( mesh % Variables, readDataName)
        DO i = 1, mesh % NumberOfNodes
            j = BCPerm(i)
            IF(j == 0) CYCLE
            readDataVariable % Values(readDataVariable % Perm(i)) = 1 + ( (CoordVals(3*j-2)) * (CoordVals(3*j-2)) ) &
                                   + 2 * ( (CoordVals(3*j-1)) * (CoordVals(3*j-1)) )
            write(infoMessage,'(A,I5,A,I5,A,F10.4,A,F10.2,A,F10.2)') 'Node: ',i,' Index: ',j,' Value: ', &
                        readDataVariable % Values(readDataVariable % Perm(i)), &
                        ' X= ', CoordVals(3*j-2), ' Y= ', CoordVals(3*j-1)   
            CALL Info('CouplerSolver',infoMessage)
            
        END DO

        writeDataVariable  => VariableGet( mesh % Variables, writeDataName)
        ! writeDataVariable % Values = 7
        CALL Info('CouplerSolver','Printing write Data')
        DO i = 1, mesh % NumberOfNodes
            j = BCPerm(i)
            IF(j == 0) CYCLE
            
            write(infoMessage,'(A,I5,A,I5,A,F10.4,A,F10.2,A,F10.2)') 'Node: ',i,' Index: ',j,' Value: ', &
                                writeDataVariable % Values(writeDataVariable % Perm(i)), &
                                ' X= ', CoordVals(3*j-2), ' Y= ', CoordVals(3*j-1)   
            CALL Info('CouplerSolver',infoMessage)
        END DO

    
    end select

    CALL Info('CouplerSolver',' Ended '//achar(27)//'[0m.')
END SUBROUTINE CouplerSolver