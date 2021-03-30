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
    CHARACTER(LEN=MAX_NAME_LEN) :: VarName, MaskName
    TYPE(Variable_t), POINTER :: Var
    TYPE(Mesh_t), POINTER :: Mesh
    TYPE(ValueList_t), POINTER :: Params
    REAL(KIND=dp), POINTER :: InterfaceVals(:), CoordVals(:)
    INTEGER, POINTER :: BCPerm(:)
    INTEGER :: i,j,k,nsize,dofs,rank
    LOGICAL :: DifferentPerm, Import, Export, Found
    LOGICAL :: Visited = .FALSE.
    !------------------------------------------------------------------------------
    CHARACTER*512                   :: config
    CHARACTER*50                    :: participantName, meshName, writeInitialData, readItCheckp, writeItCheckp
    INTEGER                         :: commsize, ongoing, dimensions, meshID, vertexID, bool
    REAL                            :: dtlimit
    REAL, DIMENSION(:), ALLOCATABLE :: vertex  

    SAVE BCPerm, nsize, CoordVals, Visited

    ! Constants in f90 have to be prefilled with blanks to be compatible with preCICE
    writeInitialData(1:50)='                                                  '
    readItCheckp(1:50)='                                                  '
    writeItCheckp(1:50)='                                                  '

    CALL precicef_action_write_initial_data(writeInitialData)
    CALL precicef_action_read_iter_checkp(readItCheckp)
    CALL precicef_action_write_iter_checkp(writeItCheckp)


    CALL Info(''//achar(27)//'[31mCouplerSolver ', 'Transfering results between different software ')

    

    Mesh => Solver % Mesh
    Params => GetSolverParams()

    rank = 0
    commsize = 1

    !------------------------------------------------------------------------------
    ! Create Coupling Interface
    ! IF solver has not been visited create the coupling interface
    IF(.NOT. Visited ) THEN
      ! For the BC where coupler active set "Coupler Interface = .TRUE."
      MaskName = 'Coupler Interface'
      NULLIFY( BCPerm )    
      ALLOCATE( BCPerm( Mesh % NumberOfNodes ) )
      BCPerm = 0
      CALL MakePermUsingMask( Model, Solver, Mesh, MaskName, .FALSE., &
          BCPerm, nsize )! , ParallelComm = .TRUE. )
      CALL Info('CouplerSolver','Number of nodes at interface:'//TRIM(I2S(nsize)))
      ! This is MPI rank [0,ParEnv % PEs-1]
      rank = ParEnv % MyPe    
      
      ! Also save the coordinates at the interface
      ALLOCATE( CoordVals(3*nsize) )
      DO i=1,Mesh % NumberOfNodes
          j = BCPerm(i)
          CoordVals(3*j-2) = Mesh % Nodes % x(i)
          CoordVals(3*j-1) = Mesh % Nodes % y(i)
          CoordVals(3*j) = Mesh % Nodes % z(i)
      END DO
      CALL Info('CouplerSolver','Created nodes at interface')




    !------------------------------------------------------------------------------
      CALL Info('CouplerSolver','Creating Precice Interface')

      participantName = ListGetString(Params,'Participant Name',Found )
      IF(.NOT. Found ) THEN
        CALL Fatal('CouplerSolver','Give  "Participant Name" ')
      END IF

      meshName = ListGetString(Params,'Mesh Name Precice',Found )

      IF(.NOT. Found ) THEN
        CALL Fatal('CouplerSolver','Give  "Mesh Name" ')
      END IF
      PRINT *,participantName,meshName
    !   CALL precicef_create(participantName, './precice-config.xml', rank, commsize)
      CALL precicef_create('Lower_Plate', './precice-config.xml', rank, commsize)
      ! Allocate dummy mesh with only one vertex 
    !   CALL precicef_get_dims(dimensions)
    !   ALLOCATE(vertex(dimensions))
    !   vertex = 0
    !   CALL precicef_get_mesh_id(meshName, meshID)
    !   CALL precicef_set_vertex(meshID, vertex, vertexID)  
    !   DEALLOCATE(vertex) 

    !   CALL precicef_initialize(dtlimit)
    !   CALL precicef_action_required(writeInitialData, bool)
    !   IF (bool.EQ.1) THEN
    !     WRITE (*,*) 'DUMMY: Writing initial data'
    !   ENDIF
    !   CALL precicef_initialize_data()
    !------------------------------------------------------------------------------





      Visited = .TRUE.
    END IF
    !------------------------------------------------------------------------------
      
      
      
    !------------------------------------------------------------------------------
    ! If not import, then it is export: we must have either field
    ! We can basically use the same module for both import and export  
      VarName = ListGetString(Params,'Import Field',Import )
      IF(.NOT. Import ) THEN
        VarName = ListGetString(Params,'Export Field',Export )
        IF(.NOT. Export ) THEN
            CALL Fatal('CouplerSolver','Give eiher "Import Field" or "Export Field"')
        END IF
      END IF
    !------------------------------------------------------------------------------ 


    !------------------------------------------------------------------------------ 
    ! The variable might not exist, this needs to be done only once.
    ! Thereafter this variable exists for all uses.  
      Var => VariableGet( Mesh % Variables, VarName ) 
      IF(ASSOCIATED( Var ) ) THEN
        CALL Info('CouplerSolver','Using existing variable: '//TRIM(VarName))
      ELSE
        CALL Info('CouplerSolver','Creating variable as it does not exist: '//TRIM(VarName))
        Dofs = ListGetInteger( Params,'Field Dofs',Found )
        IF(.NOT. Found ) Dofs = 1
        CALL VariableAddVector( Mesh % Variables, Mesh, Solver, VarName, DOFs, &
            Perm = BCPerm, Secondary = .TRUE. )
        Var => VariableGet( Mesh % Variables, VarName ) 
      END IF

      dofs = Var % Dofs
    !------------------------------------------------------------------------------ 

    !------------------------------------------------------------------------------ 
    DifferentPerm = ANY( Var % Perm /= BCPerm )
    IF( DifferentPerm ) THEN
        ! If the field we are mapping to is of different size then
        ! we need temporal field were to map. 
        CALL Info('CouplerSolver','Creating temporal data on interface')
        ALLOCATE( InterfaceVals(nsize*dofs) )
        InterfaceVals = 0.0_dp
    ELSE
        ! Otherwise we may just use the vector directly
        CALL Info('CouplerSolver','Variable is exactly on the interface')
        InterfaceVals => Var % Values
    END IF
    !------------------------------------------------------------------------------ 

    !------------------------------------------------------------------------------ 
    ! Implement some external method that gets values from different software
    IF( Import ) THEN       
        DO i=1,nsize
            InterfaceVals(i) = 0      
            PRINT *,'import value:',i,InterfaceVals(i),'at',CoordVals(3*i-2:3*i)
        END DO 
        
        ! Pick only those nodal values that are on the interface to the import field. 
        IF( DifferentPerm ) THEN
            DO i=1, Mesh % NumberOfNodes
                j = BCPerm(i)
                IF(j == 0) CYCLE
                Var % Values(Var % Perm(i)) = InterfaceVals(j)
            END DO
        END IF
    ELSE
        ! Pick the values on the interface to export stuff.
        IF( DifferentPerm ) THEN
            DO i=1, Mesh % NumberOfNodes
                j = BCPerm(i)
                IF(j == 0) CYCLE
                InterfaceVals(j) = Var % Values(Var % Perm(i))
            END DO
        END IF
        DO i=1,nsize
            PRINT *,'export value:',i,InterfaceVals(i),'at',CoordVals(3*i-2:3*i)
        END DO
    END IF

    IF( DifferentPerm ) THEN
        DEALLOCATE( InterfaceVals )
    END IF
    
    IF( Import ) THEN
        CALL Info('CouplerSolver','Data import done!')
    ELSE
        CALL Info('CouplerSolver','Data export done!')
    END IF
      
    CALL Info('CouplerSolver',' Ended '//achar(27)//'[0m.')

END SUBROUTINE CouplerSolver