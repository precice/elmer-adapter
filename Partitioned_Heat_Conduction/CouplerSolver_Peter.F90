!-----------------------------------------------------------------------------
!> This is a dummy skeleton for interface data exchange. 
!------------------------------------------------------------------------------
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
  INTEGER, POINTER :: BCPerm(:)
  INTEGER :: i,j,k,nsize,dofs,rank
  REAL(KIND=dp), POINTER :: InterfaceVals(:), CoordVals(:)
  LOGICAL :: DifferentPerm, DoImport, Found
  TYPE(ValueList_t), POINTER :: Params
  LOGICAL :: Visited = .FALSE.
  
  SAVE BCPerm, nsize, CoordVals, Visited
  
  CALL Info('CouplerSolver','Transfering results between different software')
  
  Mesh => Solver % Mesh
  Params => GetSolverParams()

  ! IF solver has not been visited create the coupling interface
  IF(.NOT. Visited ) THEN
    MaskName = 'Coupler Interface' ! For the BC where coupler active set "Coupler Interface = .TRUE."
    ! Permutation for the coupling interface
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
    Visited = .TRUE.
  END IF
  
  
  ! If not import, then it is export: we must have either field
  ! We can basically use the same module for both import and export
  VarName = ListGetString(Params,'Import Field',DoImport )
  IF(.NOT. DoImport ) THEN
    VarName = ListGetString(Params,'Export Field',Found )
    IF(.NOT. Found ) THEN
      CALL Fatal('CouplerSolver','Give eiher "Import Field" or "Export Field"')
    END IF
  END IF

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
       
  ! Implement some external method that gets values from different software
  IF( DoImport ) THEN       
    DO i=1,nsize
      InterfaceVals(i) = CoordVals(3*i-2) + 0.2 * CoordVals(3*i-1)      
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

  IF( DoImport ) THEN
    CALL Info('CouplerSolver','Data import done!')
  ELSE
    CALL Info('CouplerSolver','Data export done!')
  END IF
  
!------------------------------------------------------------------------------
END SUBROUTINE CouplerSolver
!------------------------------------------------------------------------------
