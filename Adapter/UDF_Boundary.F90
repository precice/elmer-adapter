!-------------------------------
!  Inlet Boundary condition v
!  velocity in x-direction
!------------------------------


Function SetFlux(model,n, dummy_argument) RESULT(HeatFlux)

    USE DefUtils

    IMPLICIT None

    TYPE(Model_t) :: model
    INTEGER       :: n
    REAL(KIND=dp) :: dummy_argument,HeatFlux
    REAL(KIND=dp) :: x,y
    REAL(KIND=dp), dimension (10) :: Flux

    CHARACTER(LEN=MAX_NAME_LEN)         :: MaskName = "Coupler Interface"
    TYPE(Mesh_t), POINTER               :: mesh
    TYPE(Solver_t),Pointer              :: Solver
    INTEGER, POINTER                    :: BCPerm(:)
    INTEGER                             :: vertexSize
    LOGICAL                             :: Visited = .FALSE.

    SAVE Visited,BCPerm

    Solver => model % Solver
    Mesh => Solver % Mesh


    x = model % Nodes % x(n)
    y = model % Nodes % y(n)

    ! Flux = (/-2.104,-2.307,-2.388,-2.439,-2.458,-2.450,-2.4133,-2.339,-2.202,-2.008/)
    Flux = (/-2.008,-2.202,-2.339,-2.4133,-2.450,-2.458,-2.439,-2.388,-2.307,-2.104/)
    
   

    IF(.NOT. Visited) THEN

        NULLIFY( BCPerm )    
        ALLOCATE( BCPerm( Mesh % NumberOfNodes ) )
        BCPerm = 0
    
        CALL MakePermUsingMask( Model, Solver, Mesh, MaskName, .TRUE., &
            BCPerm, vertexSize )
        Visited = .TRUE.
    END IF

    ! HeatFlux = Flux(BCPerm(n)) 
    HeatFlux = -1.9
    !Print *,HeatFlux,n,BCPerm(n),Flux(BCPerm(n))

    ! LOGICAL       :: Visited = .FALSE.

    ! INTEGER                         :: vertexSize
    ! INTEGER, POINTER                    :: BCPerm(:)
    ! TYPE(Mesh_t), POINTER               :: mesh
    ! TYPE(Solver_t) :: Solver
    ! CHARACTER(LEN=MAX_NAME_LEN)         :: maskName

    ! SAVE Visited,BCPerm

    ! IF (.NOT. Visited) THEN
        
    !     maskName = 'Coupler Interface'

    !     Solver = model % Solver    
    !     Mesh => Solver % Mesh

    !     NULLIFY( BCPerm )    
    !     ALLOCATE( BCPerm( Mesh % NumberOfNodes ) )
    !     BCPerm = 0
    !     ! CALL MakePermUsingMask( Model, Solver, Mesh, MaskName, .FALSE., &
    !     !     BCPerm, vertexSize )
    !     CALL MakePermUsingMask( Model, Solver, Mesh, MaskName, .TRUE., &
    !         BCPerm, vertexSize )
    !     CALL Info('CouplerSolver','Number of nodes at interface:'//TRIM(I2S(vertexSize)))
    !     Visited = .TRUE.
    ! END IF

    ! vertecies = (/1.3683,-2.8982,-1.1475,-0.5913,-0.3677,-0.2596,-0.2236,-0.2578,-0.4033,-0.9204,-0.5099/)
    
    ! ! Print *, n,BCPerm(n)

    ! tempLoads = vertecies(BCPerm(n))
    

END FUNCTION SetFlux