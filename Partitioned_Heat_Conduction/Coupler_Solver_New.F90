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

    !--------------------------Precice-Variables-------------------------------------
    !-------------------------Strings----------------------------------------------
    CHARACTER(LEN=MAX_NAME_LEN)         :: participantName, meshName, configPath, readDataName,writeDataName
    !-------------------------IDs-Integer----------------------------------------------
    INTEGER                         :: meshID,readDataID, writeDataID
    !------------------------Data Arrays----------------------------------------------
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: writeData, readData
    INTEGER, POINTER                :: vertexIDs(:)
    !------------------------Mesh Data----------------------------------------------
    INTEGER                         :: vertexSize
    INTEGER                         :: Dofs

    !--------------------------Variables-End-------------------------------------------



    !--------------------------SAVE-Start-------------------------------------------
    SAVE meshID,readDataID,writeDataID
    SAVE meshName,readDataName,writeDataName
    SAVE itask
    SAVE BCPerm,CoordVals

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

    case(1)
        !-- First Time Visit, Create Precice, create nodes at interface

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
        CALL MakePermUsingMask( Model, Solver, Mesh, MaskName, .FALSE., &
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
            
            write(infoMessage,'(A,I5,A,I5,A,F10.2)') 'Node: ',i,' Index: ',j,' Value: ',readDataVariable % Values(i) 
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
        CALL Info('CouplerSolver','Printing write Data')
        DO i = 1, mesh % NumberOfNodes
            j = BCPerm(i)
            IF(j == 0) CYCLE
            
            write(infoMessage,'(A,I5,A,I5,A,F10.2)') 'Node: ',i,' Index: ',j,' Value: ',writeDataVariable % Values(i)
            CALL Info('CouplerSolver',infoMessage)
        END DO

        !------------------------------------------------------------------------------
        itask = 2

    case(2)
        readDataVariable  => VariableGet( mesh % Variables, readDataName)
        CALL Info('CouplerSolver','Printing read Data')
        DO i = 1, mesh % NumberOfNodes
            j = BCPerm(i)
            IF(j == 0) CYCLE
            
            write(infoMessage,'(A,I5,A,I5,A,F10.2,A,F10.2,A,F10.2)') 'Node: ',i,' Index: ',j,' Value: ' &
                            ,readDataVariable % Values(readDataVariable % Perm(i)),&
                            ' X= ', CoordVals(3*j-2), ' Y= ', CoordVals(3*j-1)
            CALL Info('CouplerSolver',infoMessage)
        END DO

        writeDataVariable  => VariableGet( mesh % Variables, writeDataName)
        CALL Info('CouplerSolver','Printing write Data')
        DO i = 1, mesh % NumberOfNodes
            j = BCPerm(i)
            IF(j == 0) CYCLE
            
            write(infoMessage,'(A,I5,A,I5,A,F10.2,A,F10.2,A,F10.2)') 'Node: ',i,' Index: ',j,' Value: ',&
                    writeDataVariable % Values(writeDataVariable % Perm(i)), &
                    ' X= ', CoordVals(3*j-2), ' Y= ', CoordVals(3*j-1)
            CALL Info('CouplerSolver',infoMessage)
        END DO
    end select

    CALL Info('CouplerSolver',' Ended '//achar(27)//'[0m.')
END SUBROUTINE CouplerSolver