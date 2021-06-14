!-----------------------------------------------------------------------------
!> This is a dummy skeleton for interface data exchange using Precice. 
!-----------------------------------------------------------------------------
SUBROUTINE CouplerSolver( Model,Solver,dt,TransientSimulation )
    !------------------------------------------------------------------------------
    USE DefUtils

    IMPLICIT NONE
    !------------------------------------------------------------------------------
    TYPE(Solver_t) :: Solver
    TYPE(Model_t) :: Model
    REAL(KIND=dp) :: dt
    LOGICAL :: TransientSimulation
    
    !------------------------------------------------------------------------------
    ! Elmer Variables required for data transfer
    TYPE(Mesh_t), POINTER               :: Mesh
    TYPE(Variable_t), POINTER           :: ImportVar,ExportVar
    TYPE(ValueList_t), POINTER          :: Simulation
    TYPE(ValueList_t), POINTER          :: Params
    CHARACTER(LEN=MAX_NAME_LEN)         :: MaskName
    REAL(KIND=dp), POINTER              :: CoordVals(:)
    INTEGER, POINTER                    :: BCPerm(:)
    INTEGER                             :: i,j,k,nsize,dofs
    LOGICAL                             :: Found
    LOGICAL                             :: Visited = .FALSE.

    !------------------------------------------------------------------------------
    ! Precice variables for using Precice API
    
    CHARACTER*50                    :: writeInitialData, readItCheckp, writeItCheckp
    CHARACTER*50                    :: readDataName, writeDataName!Done
    CHARACTER*50                    :: printMessage!Done
    INTEGER                         :: ongoing,   vertexID, bool,numberOfVertices
    INTEGER                         :: itask = 1!Done
    REAL(KIND=dp)                   :: dtlimit, timeStep,interval,temp
    INTEGER                         :: timeInterval
    REAL, DIMENSION(:), ALLOCATABLE :: vertex  

    !------------------------------------------------------------------------------
    CHARACTER*512                   :: config!DONE
    CHARACTER*50                    :: participantName, meshName!Done
    INTEGER                         :: rank,commsize,dim,meshID,VertexSize!Done
    INTEGER                         :: temperatureID,fluxID!Done, here repeated
    REAL(KIND=dp)                   :: precice_dt,time_step,time_interval
    INTEGER, POINTER                :: vertexIDs(:)!Done
    REAL(KIND=dp), POINTER          :: temperature(:), flux(:)!Done
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: writeData, readData!Done

    INTEGER                         :: readDataID, writeDataID!Done

    SAVE Visited,rank,commsize,time_step,time_interval
    
    SAVE dim,meshID
    SAVE temperature,flux
    SAVE writeItCheckp,readItCheckp

    SAVE config, participantName, meshName, MaskName
    SAVE itask
    SAVE readDataID,writeDataID

    
    CALL Info(''//achar(27)//'[31mCouplerSolver ', 'Transfering results between different software ')

    Simulation => GetSimulation()
    Mesh => Solver % Mesh
    Params => GetSolverParams()

    rank = 0
    commsize = 1
    
    select case(itask)

    case(1)
      !-- First Time Visit, Create Precice, create nodes at interface

      !---------------Setting Mask & Precice Parameter--------------------
        MaskName = 'Coupler Interface'
        participantName = GetString( Simulation, 'participantName', Found )
        meshName = GetString( Simulation, 'meshName', Found )
        config = GetString( Simulation, 'config', Found )
        time_interval = dble(GetInteger(Simulation,'Timestep intervals',Found))
      !--------------------------------------------------------------------

      !---------------Allocate Interface-----------------------------------
        NULLIFY( BCPerm )    
        ALLOCATE( BCPerm( Mesh % NumberOfNodes ) )
        BCPerm = 0
        ! ?? I do not understand what BCPerm is doing
        CALL MakePermUsingMask( Model, Solver, Mesh, MaskName, .FALSE., &
              BCPerm, VertexSize)
        
        Print *, "VertexSize = ", VertexSize      
        CALL Info('CouplerSolver','Number of nodes at interface:'//TRIM(I2S(VertexSize)))
                
        
        ! ! Also save the coordinates at the interface
        ALLOCATE( CoordVals(3*VertexSize) )
        ALLOCATE(vertexIDs(VertexSize))
        

        DO i=1,Mesh % NumberOfNodes
            j = BCPerm(i)
            CoordVals(3*j-2) = Mesh % Nodes % x(i)
            CoordVals(3*j-1) = Mesh % Nodes % y(i)
            CoordVals(3*j) = Mesh % Nodes % z(i)
            IF(j /= 0) THEN
                vertexIDs(j) = j
            END IF
        END DO
        CALL Info('CouplerSolver','Created nodes at interface')

      !--------------------------------------------------------------------

        Print *, "PRECICE create"
        time_step = 1
        ! TODO implement to handle multiple problems, not to be hardcoded
        readDataName = ListGetString(Params,'ImportField',Found )

        IF(.NOT. Found ) THEN
          CALL Fatal('CouplerSolver', 'No Import Field Specified')
        END IF

        writeDataName = ListGetString(Params,'ExportField',Found )

        IF(.NOT. Found ) THEN
          CALL Fatal('CouplerSolver', 'No Export Field Specified')
        END IF

        Print *, "readDataName  ", readDataName
        Print *, "writeDataName ", writeDataName

        ImportVar => VariableGet( Mesh % Variables, readDataName ) 
        IF(ASSOCIATED( ImportVar ) ) THEN
          CALL Info('CouplerSolver','Using existing variable: '//TRIM(readDataName))
        ELSE
          CALL Info('CouplerSolver','Creating variable as it does not exist: '//TRIM(readDataName))
          Dofs = ListGetInteger( Params,'Field Dofs',Found )
          IF(.NOT. Found ) Dofs = 1
          CALL VariableAddVector( Mesh % Variables, Mesh, Solver, readDataName, DOFs, &
              Perm = BCPerm, Secondary = .TRUE. )
          ImportVar => VariableGet( Mesh % Variables, readDataName ) 
        END IF

        ExportVar => VariableGet( Mesh % Variables, writeDataName ) 
        IF(ASSOCIATED( ImportVar ) ) THEN
          CALL Info('CouplerSolver','Using existing variable: '//TRIM(writeDataName))
        ELSE
          CALL Info('CouplerSolver','Creating variable as it does not exist: '//TRIM(writeDataName))
          Dofs = ListGetInteger( Params,'Field Dofs',Found )
          IF(.NOT. Found ) Dofs = 1
          CALL VariableAddVector( Mesh % Variables, Mesh, Solver, writeDataName, DOFs, &
              Perm = BCPerm, Secondary = .TRUE. )
          ExportVar => VariableGet( Mesh % Variables, writeDataName ) 
        END IF

        CALL precicef_create(participantName, config, rank, commsize)

        writeInitialData(1:50)='                                                  '
        readItCheckp(1:50)='                                                  '
        writeItCheckp(1:50)='                                                  '


        CALL precicef_action_write_initial_data(writeInitialData)
        CALL precicef_action_read_iter_checkp(readItCheckp)
        CALL precicef_action_write_iter_checkp(writeItCheckp)
        
        CALL precicef_get_dims(dim)
        CALL precicef_get_mesh_id(meshName, meshID)
        CALL precicef_set_vertices(meshID, VertexSize, CoordVals, vertexIDs)

        CALL precicef_get_data_id(readDataName,meshID,readDataID)
        CALL precicef_get_data_id(writeDataName,meshID,writeDataID)
        

        ALLOCATE(readData(VertexSize))
        ALLOCATE(writeData(VertexSize))

        readData = 0
        writeData = 0
        

        CALL precicef_initialize(dt)
        CALL precicef_is_action_required(writeInitialData, bool)
        IF (bool.EQ.1) THEN
          WRITE (*,*) 'DUMMY: Writing initial data'
        ENDIF
        CALL precicef_initialize_data()
      
        CALL precicef_is_coupling_ongoing(ongoing)
        itask = 2

        
    case(2)
        !-- time loop
        ! CALL precicef_is_action_required(writeItCheckp, bool)
      
        ! IF (bool.EQ.1) THEN
        !   WRITE (*,*) 'DUMMY: Writing iteration checkpoint'
        !   CALL precicef_mark_action_fulfilled(writeItCheckp)
        ! ENDIF
      
        ! CALL precicef_is_read_data_available(bool)
        
        ! IF (bool.EQ.1) THEN
        ! !   CALL precicef_read_bvdata(readDataID, numberOfVertices, vertexIDs, readData)
        !   CALL precicef_read_bsdata(readDataID, numberOfVertices, vertexIDs, readData)
        ! ENDIF
      
        ! WRITE (*,*) 'readData: ', readData
      
        ! ! writeData = readData + 1
      
        ! CALL precicef_is_write_data_required(dt, bool)
        ! IF (bool.EQ.1) THEN
        ! !   CALL precicef_write_bvdata(writeDataID, numberOfVertices, vertexIDs, writeData)
        !   CALL precicef_write_bsdata(writeDataID, numberOfVertices, vertexIDs, writeData)
        ! ENDIF
      
        ! CALL precicef_advance(dt)
      
        ! CALL precicef_is_action_required(readItCheckp, bool)
        ! IF (bool.EQ.1) THEN
        !   WRITE (*,*) 'DUMMY: Reading iteration checkpoint'
        !   CALL precicef_mark_action_fulfilled(readItCheckp)
        ! ELSE
        !   WRITE (*,*) 'DUMMY: Advancing in time'
        ! ENDIF
      
        ! CALL precicef_is_coupling_ongoing(ongoing)

        ! IF(ongoing.EQ.0) THEN
        !     itask = 3
        ! END IF
    
    case(3)
        print *, "PRECICE finalize"
    end select

    !TODO implement better way to acquire time step
    ! select case(itask)

    !   case(1)
    !     !
    !     ! Create preCICE
    !     !

    !     Print *, "PRECICE create"
    !     time_step = 1

    !     ! Acquiring participant data
    !     participantName = GetString( Simulation, 'participantName', Found )
    !     meshName = GetString( Simulation, 'meshName', Found )
    !     config = GetString( Simulation, 'config', Found )

    !     time_interval = dble(GetInteger(Simulation,'Timestep intervals',Found))

    !     ! CALL precicef_create(participantName, config, rank, commsize)
        
    !     ! writeInitialData(1:50)='                                                  '
    !     ! readItCheckp(1:50)='                                                  '
    !     ! writeItCheckp(1:50)='                                                  '


    !     ! CALL precicef_action_write_initial_data(writeInitialData)
    !     ! CALL precicef_action_read_iter_checkp(readItCheckp)
    !     ! CALL precicef_action_write_iter_checkp(writeItCheckp)

    !     NULLIFY( BCPerm )    
    !     ALLOCATE( BCPerm( Mesh % NumberOfNodes ) )
    !     BCPerm = 0
    !     ! ?? I do not understand what BCPerm is doing
    !     CALL MakePermUsingMask( Model, Solver, Mesh, MaskName, .FALSE., &
    !           BCPerm, VertexSize)
        
    !     CALL Info('CouplerSolver','Number of nodes at interface:'//TRIM(I2S(VertexSize)))
                
        
    !     ! ! Also save the coordinates at the interface
    !     ALLOCATE( CoordVals(3*VertexSize) )
    !     ALLOCATE(vertexIDs(VertexSize))

    !     DO i=1,Mesh % NumberOfNodes
    !         j = BCPerm(i)
    !         CoordVals(3*j-2) = Mesh % Nodes % x(i)
    !         CoordVals(3*j-1) = Mesh % Nodes % y(i)
    !         CoordVals(3*j) = Mesh % Nodes % z(i)
    !         IF(j /= 0) THEN
    !             vertexIDs(j) = j
    !         END IF
    !     END DO
    !     CALL Info('CouplerSolver','Created nodes at interface')
        
    !     ! CALL precicef_get_dims(dim)
    !     ! CALL precicef_get_mesh_id(meshName, meshID)
    !     ! CALL precicef_set_vertices(meshID, VertexSize, CoordVals, vertexIDs)

    !     ! CALL precicef_get_data_id("Temperature",meshID,temperatureID)
    !     ! CALL precicef_get_data_id("Flux",meshID,FluxID)
    !     ! ALLOCATE( temperature(VertexSize) )
    !     ! ALLOCATE( flux(VertexSize) )
    !     ! temperature = 0
    !     ! flux = 0
    !     ! CALL precicef_initialize(dt)
    !     ! CALL precicef_is_action_required(writeInitialData, bool)
    !     ! IF (bool.EQ.1) THEN
    !     !   WRITE (*,*) 'DUMMY: Writing initial data'
    !     ! ENDIF
    !     ! CALL precicef_initialize_data()
      
    !     ! CALL precicef_is_coupling_ongoing(ongoing)
    !   !   itask = 2
    !   ! case(2)
    !   !   time_step = time_step + dt
    !   !   Print *, time_step
    !   !   temp = time_step + dt
    !   !   ! Print *,temp
    !   !   ! Print *,(temp .eq. time_interval)
    !   !   IF(temp .eq. time_interval) THEN
          
    !   !     itask = 5
    !   !     Print *,itask
    !   !   END IF

    !   case(5)
    !     !
    !     ! Finalize preCICE, called by nsi_turnof
    !     !
    !     print *, "PRECICE finalize"
    !     ! call precicef_finalize()
    !   end select

    CALL Info('CouplerSolver',' Ended '//achar(27)//'[0m.')
    
   
    
END SUBROUTINE CouplerSolver
