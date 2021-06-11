PROGRAM main
    IMPLICIT NONE
    CHARACTER*512           :: config
    CHARACTER*50            :: participantName, meshName, writeInitialData,readItCheckp, writeItCheckp
    CHARACTER*50            :: readDataName, writeDataName
    INTEGER                 :: rank, commsize, ongoing, dimensions, meshID, bool, numberOfVertices, i,j
    INTEGER                 :: readDataID, writeDataID
    DOUBLE precision        :: dt
    ! write and read data vectors to hold transferable data
    DOUBLE PRECISION,DIMENSION(:), ALLOCATABLE :: vertices, writeData, readData
    INTEGER, DIMENSION(:), ALLOCATABLE :: vertexIDs

! Constants in f90 have to be prefilled with blanks to be compatible with preCICE
  writeInitialData(1:50)='                                                  '
  readItCheckp(1:50)='                                                  '
  writeItCheckp(1:50)='                                                  '

! ?? I do not know why this for
  CALL precicef_action_write_initial_data(writeInitialData)
  CALL precicef_action_read_iter_checkp(readItCheckp)
  CALL precicef_action_write_iter_checkp(writeItCheckp)

  WRITE (*,*) 'DUMMY: Starting Fortran solver dummy...'
  ! Method calls to acquire parameter passed through command line
  CALL getarg(1, config)
  CALL getarg(2, participantName)
  CALL getarg(3, meshName)

  ! checking which solver is this and setting the read and write for each one
  ! for example, in our case, for dirichlet it writes temperature and read flux
  ! while neumann write flux and read temperature
  IF(participantName .eq. 'SolverOne') THEN
    writeDataName = 'dataOne'
    readDataName = 'dataTwo'
  ENDIF
  IF(participantName .eq. 'SolverTwo') THEN
    writeDataName = 'dataTwo'
    readDataName = 'dataOne'
  ENDIF

  rank = 0
  commsize = 1
  dt = 1
  ! ?? why number of vertices is 3
  numberOfVertices = 3
  CALL precicef_create(participantName, config, rank, commsize)

  CALL precicef_get_dims(dimensions)
  ALLOCATE(vertices(numberOfVertices*dimensions))
  ALLOCATE(vertexIDs(numberOfVertices))
  ALLOCATE(readData(numberOfVertices*dimensions))
  ALLOCATE(writeData(numberOfVertices*dimensions))
  CALL precicef_get_mesh_id(meshName, meshID)

  do i = 1,numberOfVertices,1
    do j = 1,dimensions,1
      vertices((i - 1)*dimensions + j ) = i-1
      readData((i - 1)*dimensions + j ) = i-1
      writeData((i - 1)*dimensions + j ) = i-1
    enddo
    vertexIDs(i) = i-1
  enddo

  CALL precicef_set_vertices(meshID, numberOfVertices, vertices, vertexIDs)
  DEALLOCATE(vertices)

  CALL precicef_get_data_id(readDataName,meshID,readDataID)
  CALL precicef_get_data_id(writeDataName,meshID,writeDataID)

  CALL precicef_initialize(dt)

  CALL precicef_finalize()
  WRITE (*,*) 'DUMMY: Closing Fortran solver dummy...'

  DEALLOCATE(writeData)
  DEALLOCATE(readData)
  DEALLOCATE(vertexIDs)

END PROGRAM