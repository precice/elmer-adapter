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
    INTEGER                         :: itask = 1
    REAL(KIND=dp)                   :: dtlimit, timeStep,interval,temp
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
    
    SAVE dim,meshID
    SAVE temperature,flux
    SAVE writeItCheckp,readItCheckp

    SAVE config, participantName, meshName, MaskName
    SAVE itask

    
    CALL Info(''//achar(27)//'[31mCouplerSolver ', 'Transfering results between different software ')

    Simulation => GetSimulation()
    Mesh => Solver % Mesh
    Params => GetSolverParams()

    rank = 0
    commsize = 1
    
    select case(itask)

    case(1)
      !-- Create Precice
        NULLIFY( BCPerm )    
        ALLOCATE( BCPerm( Mesh % NumberOfNodes ) )
        BCPerm = 0
        ! ?? I do not understand what BCPerm is doing
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
