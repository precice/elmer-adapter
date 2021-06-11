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
    TYPE(ValueList_t), POINTER          :: Simulation
    TYPE(ValueList_t), POINTER          :: Params
    CHARACTER(LEN=MAX_NAME_LEN)         :: MaskName
    REAL(KIND=dp), POINTER              :: CoordVals(:)
    INTEGER, POINTER                    :: BCPerm(:)
    INTEGER                             :: i,j,k,nsize
    LOGICAL                             :: Found
    LOGICAL                             :: Visited = .FALSE.

    !------------------------------------------------------------------------------
    ! Precice variables for using Precice API
    
    CHARACTER*50                    :: writeInitialData, readItCheckp, writeItCheckp
    CHARACTER*50                    :: readDataName, writeDataName
    CHARACTER*50                    :: printMessage
    INTEGER                         :: ongoing,   vertexID, bool,numberOfVertices
    REAL(KIND=dp)                   :: dtlimit, timeStep,interval
    INTEGER                         :: timeInterval
    REAL, DIMENSION(:), ALLOCATABLE :: vertex  

    !------------------------------------------------------------------------------
    CHARACTER*512                   :: config
    CHARACTER*50                    :: participantName, meshName
    INTEGER                         :: rank,commsize,dim,meshID,VertexSize
    INTEGER                         :: temperatureID,fluxID
    REAL(KIND=dp)                   :: precice_dt,time_step,time_interval
    INTEGER, POINTER                :: vertexIDs(:)
    REAL(KIND=dp), POINTER          :: temperature(:), flux(:)
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: writeData, readData

    SAVE Visited,rank,commsize,time_step,time_interval
    SAVE config, participantName, meshName, MaskName
    SAVE dim,meshID
    SAVE temperature,flux
    SAVE writeItCheckp,readItCheckp

    CALL Info(''//achar(27)//'[31mCouplerSolver ', 'Transfering results between different software ')

    Simulation => GetSimulation()
    Mesh => Solver % Mesh
    Params => GetSolverParams()

    


    !------------------------------------------------------------------------------
    ! Precice initialization, only done once
    ! IF(.NOT.Visited) THEN
        ! Setting the required instanse parameters
        config = '../precice-config.xml'
        participantName = 'Neumann'
        meshName = 'Neumann-Mesh'
        MaskName = 'Coupler Interface'

        rank = 0
        commsize = 1
        timestep = 0


        writeInitialData(1:50)='                                                  '
        readItCheckp(1:50)='                                                  '
        writeItCheckp(1:50)='                                                  '

        CALL precicef_action_write_initial_data(writeInitialData)
        CALL precicef_action_read_iter_checkp(readItCheckp)
        CALL precicef_action_write_iter_checkp(writeItCheckp)

        NULLIFY( BCPerm )    
        ALLOCATE( BCPerm( Mesh % NumberOfNodes ) )
        BCPerm = 0

        CALL MakePermUsingMask( Model, Solver, Mesh, MaskName, .FALSE., &
              BCPerm, VertexSize)
        
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

        time_interval = dble(GetInteger(Simulation,'Timestep intervals',Found))
        

        ! instantiate precice class and initialize precice with timestep
        CALL precicef_create(participantName, config, rank, commsize)

        

        CALL precicef_get_dims(dim)
        CALL precicef_get_mesh_id(meshName, meshID)
        CALL precicef_set_vertices(meshID, VertexSize, CoordVals, vertexIDs)


        CALL precicef_get_data_id("Temperature",meshID,temperatureID)
        CALL precicef_get_data_id("Flux",meshID,FluxID)
        ALLOCATE( temperature(VertexSize) )
        ALLOCATE( flux(VertexSize) )

        temperature = 0
        flux = 0

        CALL precicef_initialize(dt)

        ! CALL precicef_action_required(writeInitialData, bool)
        ! IF (bool.EQ.1) THEN
        !   WRITE (*,*) 'DUMMY: Writing initial data'
        ! ENDIF
        ! CALL precicef_initialize_data()
    
        CALL precicef_ongoing(ongoing)

        Visited = .TRUE.
    ! END IF


    !----------------------Time Loop Start----------------------------------------
    ! time loop, no need for while loop since user defined solver is already inside solver time loop
    CALL precicef_action_required(writeItCheckp, bool)
    IF (bool.EQ.1) THEN
    !   WRITE (*,*) 'DUMMY: Writing iteration checkpoint'
      CALL precicef_write_bsdata(temperatureID, vertexSize, vertexIDs, temperature);
      CALL precicef_mark_action_fulfilled(writeItCheckp)
    ENDIF

    CALL precicef_advance(dt)
    CALL precicef_ongoing(ongoing)
    time_step = time_step + dt

    ! CALL precicef_action_required(readItCheckp, bool)
    ! IF (bool.EQ.1) THEN
      WRITE (*,*) 'DUMMY: Reading iteration checkpoint'
    !   CALL precicef_read_bsdata(fluxID, vertexSize, vertexIDs, flux);
      CALL precicef_mark_action_fulfilled(readItCheckp)
    ! ELSE
    !   WRITE (*,*) 'DUMMY: Advancing in time'
    ! ENDIF



    ! CALL precicef_ongoing(ongoing)
    !     CALL precicef_read_bvdata(temperatureID, vertexSize, vertexIDs, temperature);
    
    
    ! time_step = time_step + dt
    ! CALL precicef_advance(dt)
    

    ! write(printMessage , '(A,f10.2,A,f10.2,A,f10.2)') 'dt = ' , dt,' timesteps Interval', time_interval, &
    !                                                   'timestep = ', time_step
    ! CALL Info('CouplerSolver',printMessage)

    !----------------------Time Loop End-------------------------------------------



    !------------------------------------------------------------------------------
    !After the time loop is finished precice is finalized
    IF(timeStep .eq. interval) THEN
        CALL precicef_finalize()
        DEALLOCATE(temperature)
        DEALLOCATE(Flux)
        DEALLOCATE(vertexIDs)
    END IF

    CALL Info('CouplerSolver',' Ended '//achar(27)//'[0m.')







    !------------------------------------------------------------------------------
    ! Elmer Variables required for data transfer
    ! CHARACTER(LEN=MAX_NAME_LEN) :: VarName, MaskName
    ! TYPE(Variable_t), POINTER :: Var
    ! TYPE(Mesh_t), POINTER :: Mesh
    ! TYPE(ValueList_t), POINTER :: Params
    ! TYPE(ValueList_t), POINTER :: Simulation
    ! REAL(KIND=dp), POINTER :: InterfaceVals(:), CoordVals(:)
    ! INTEGER, POINTER :: BCPerm(:)
    ! INTEGER :: i,j,k,nsize,dofs
    ! LOGICAL :: DifferentPerm, Import, Export, Found
    ! LOGICAL :: Visited = .FALSE.
    ! SAVE BCPerm, nsize, CoordVals, Visited, timeStep

    ! Constants in f90 have to be prefilled with blanks to be compatible with preCICE
    ! writeInitialData(1:50)='                                                  '
    ! readItCheckp(1:50)='                                                  '
    ! writeItCheckp(1:50)='                                                  '

    ! CALL precicef_action_write_initial_data(writeInitialData)
    ! CALL precicef_action_read_iter_checkp(readItCheckp)
    ! CALL precicef_action_write_iter_checkp(writeItCheckp)

    ! Simulation => GetSimulation()
    ! timeInterval = GetInteger(Simulation,'Timestep intervals',Found)
    ! interval = dble(timeInterval)
    ! timeInterval = ListGetConstReal(Model % Constants, 'Stefan Boltzmann ', Found)
    ! print *, Found,timeInterval
    





    
    
    






    
END SUBROUTINE CouplerSolver
