! The main subroutine that plays the game
subroutine playTicTacToe()
    integer :: i
    character, dimension(9) :: board
    LOGICAL OVER
    character (len = 1) :: winner
    board = " "
    ! print out the intro
    write (*,*) " "
    write (*,*) "PLAY TIC-TAC-TOE. ENTER 1-9 TO PLAY"
    write (*,*) "    1 | 2 | 3"
    write (*,*) "   ---+---+---"
    write (*,*) "    4 | 5 | 6"
    write (*,*) "   ---+---+---"
    write (*,*) "    7 | 8 | 9"
    write (*,*) ""
    ! original showBoard
    call showBoard(board)
    ! main loop
    do i = 1, 9
        ! user chooses move and we check if its a win
        call getMove(board)
        write (*,*) "After your move..."
        call showBoard(board)
        call CHKOVR(board, OVER, winner)
        if (OVER .EQV. .TRUE.) then ! check if user won after their turn
            if (winner .eq. 'D') then !check if draw
                write(*,*) "The game is a draw"
            else
                write (*,*) "The winner is " , winner
            end if
            exit
        end if
        ! bot chooses move and we check if its a win
        call pickMove(board)
        write (*,*) "After my move..."
        call showBoard(board)
        call CHKOVR(board, OVER, winner)
        if (OVER .EQV. .TRUE.) then !check if bot won after their turn
            if (winner .eq. 'D') then !check if draw
                write(*,*) "The game is a draw"
            else
                write (*,*) "The winner is " , winner
            end if
            exit
        end if
    end do
    return
end subroutine playTicTacToe

! simple subroutine to display the board
subroutine showBoard(board)
    character, intent(in), dimension(9) :: board
    integer :: i
    ! original tab over
    write(*,"(A)",advance='no') " "
    ! loop to print out board
    do i = 1, 9
        ! if the spot is empty print a space
        if (board(i) == '') then
            write(*,"(A)",advance='no') "   "
        else !otherwise print the value
            write (*,"(A)", advance='no') " "
            write(*,"(A)",advance='no') board(i)
            write (*,"(A)", advance='no') " "
        end if
        ! do new line if its spot 3 or 6
        if (i == 3 .or. i == 6) then
            write (*,*) ""
            write (*,*) "---+---+---"
            write(*,"(A)",advance='no') " "
        else if (i == 9) then ! put a new line if were done printing
            write (*,*) ""
        else  ! print line split
            write (*,"(A)",advance='no') "|"
        end if
    end do
    return
end subroutine showBoard

! gets the user move by taking input
! and adds it to the board
subroutine getMove(board)
    implicit none
    integer :: move
    integer :: e
    LOGICAL chkplay !makes it so we can call legacy function
    character,intent(inout),dimension(9) :: board
    do
        write (*,*) "Your move?"
        read(*,'(i10)',iostat=e) move
        if (e .eq. 1) then
            write(*,*) "wrong input please use a integer"
        end if
        if (move < 1 .or. move > 9) then
            write(*,*) "Invalid input Please choose a # from 1-9"
        else
            !compare here to make sure nothing in that spot
            if (chkplay(board,move) .EQV. .TRUE.) then !calls legacy function
                board(move) = 'X'
                exit
            else
                write(*,*) "Spot taken please choose a spot that is not occupied"
            endif
        endif
    end do
    return
end subroutine getMove

! generates a random number
subroutine getRand(board, spot)
    character,intent(inout),dimension(9) :: board
    real :: randomNumber
    integer, allocatable :: seed(:)
    integer :: size, clock, spot
    ! Random number generation loop
    do
        call random_seed(size=size) ! create seed and allocate it
        allocate(seed(size))
        call system_clock(count = clock) ! use the system clock as our seed
        seed = clock
        call random_seed(put=seed)
        deallocate(seed)
        call random_number(randomNumber) ! generate random number
        spot = int(floor(randomNumber * 10)) ! multiply by 10 and round number down
        ! if random number is out of range or the spot is taken then choose another
        if ((spot < 1 .or. spot > 9) .or. (board(spot) .ne. ' ')) then
            cycle
        else
            return
        end if
    end do
    return
end subroutine getRand

! subroutine to make a bot choose where it
! needs to go
subroutine pickMove(board)
    implicit none
    integer :: i, j, spot, stopWinSpot
    integer :: score
    character,intent(inout),dimension(9) :: board
    !!! ROWS TO WIN
    score = 0
    spot = -1
    stopWinSpot = -1
    do i=1,9 !count how many bot spots, count how many user spots and find the empty spot
        if(board(i) .eq. 'O') then
            score = score + 1
        else if(board(i) .eq. 'X') then
            score = score - 1
        else if(board(i) .eq. ' ') then
            spot = i
        end if ! if end of line
        if (mod(i,3) .eq. 0) then
            if(score .eq. 2) then !if bot can win make them win
                board(spot) = 'O'
                return
            else if(score .eq. -2) then ! if user can win stop them
                stopWinSpot = spot
            end if
            score = 0
            spot = -1
        end if
    end do

    score = 0

    !!! COLUMNS TO WIN
    j = 1
    do i = 1,9 !count how many bot spots, count how many user spots and find the empty spot
        if(board(j) .eq. 'O') then
            score = score + 1
        else if(board(j) .eq. 'X') then
            score = score - 1
        else if(board(j) .eq. ' ') then
            spot = j
        end if
        if (mod(i,3) .eq. 0) then !if end of line
            if(score .eq. 2) then !if bot can iwn make them win
                board(spot) = 'O'
                return
            else if(score .eq. - 2) then !if user can win stop them
                stopWinSpot = spot
            end if
            if (j == 7) then !start at the top of next column
                j = 2
            else if (j == 8) then !start at the top of next column
                j = 3
            end if
            score = 0
            spot = -1
        else
            j = j + 3
        end if
    end do

    score = 0
    j = 1
    !! DIAGONAL left to right
    do i = 1,3 !count how many bot spots, count how many user spots and find the empty spot
        if(board(j) .eq. 'O') then
            score = score + 1
        else if(board(j) .eq. 'X') then
            score = score - 1
        else if(board(j) .eq. ' ') then
            spot = j
        end if
        if (i .eq. 3) then !if end of line
            if(score .eq. 2) then ! if bot can win make them win
                board(spot) = 'O'
                return
            else if(score .eq. - 2) then !if user can win stop them
                stopWinSpot = spot
            end if
        else
            j = j + 4
        end if
    end do
    ! Diagonal right to left
    score = 0
    j = 3
    do i = 1,3 !count how many bot spots, count how many user spots and find the empty spot
        if(board(j) .eq. 'O') then
            score = score + 1
        else if(board(j) .eq. 'X') then
            score = score - 1
        else if(board(j) .eq. ' ') then
            spot = j
        end if
        if (i .eq. 3) then ! if end of line
            if(score .eq. 2) then !if bot can win make them win
                board(spot) = 'O'
                return
            else if(score .eq. - 2) then !if user can win stop them
                stopWinSpot = spot
            end if
        else
            j = j + 2
        end if
    end do

    !! stop the user from winning
    if (stopWinSpot .ne. -1) then
        board(stopWinSpot) = 'O'
        return
    end if
    !! if all else fails choose randomly
    call getRand(board,spot)
    board(spot) = 'O'
    return
end subroutine pickMove

! new version of chkover, checks if game is over
SUBROUTINE CHKOVR(TICTAC, OVER, WINNER)
    !
    ! CHECK IF TIC-TAC-TOE IS OVER AND DETERMINE WINNER (IF ANY)
    !
    ! ARGUMENT DEFINITIONS --
    !   INPUT ARGUMENTS
    !     TICTAC - REPRESENTS THE CURRENT STATE OF THE BOARD GAME
    !   OUTPUT ARGUMENTS
    !     OVER - INDICATES WHETHER OR NOT GAME IS OVER
    !     WINNER - INDICATES THE WINNER (O OR X) OR A DRAW (D)
    !
    CHARACTER (len=1) TICTAC(9), WINNER
    LOGICAL OVER
    !
    ! SUBROUTINE PARAMETERS
    CHARACTER (len=1) BLANK, DRAW
    PARAMETER (BLANK = ' ', DRAW = 'D')
    !
    ! FUNCTIONS USED
    LOGICAL same
    !
    ! LOCAL VARIABLES
    LOGICAL DSAME
    INTEGER IR, IC
    INTEGER i
    ! ASSUME GAME IS OVER AT START
    OVER = .TRUE.
    !
    ! CHECK FOR A WINNER
    ! CHECK ROWS FOR A WINNER
    i = 1
    DO IR=1, 3
        IF (same(TICTAC(i), TICTAC(i + 1), TICTAC(i + 2))) THEN
            WINNER = TICTAC(i)
            RETURN
        ENDIF
        i = i + 3
    END DO
    ! NO WINNER BY ROWS, CHECK COLUMNS FOR A WINNER
    i = 1
    DO IC=1, 3
        IF (same(TICTAC(i), TICTAC(i + 3), TICTAC(i + 6))) THEN
            WINNER = TICTAC(i)
            RETURN
        ENDIF
        i = i + 1
    END DO

    ! NO WINNER BY ROWS OR COLUMNS, CHECK DIAGONALS FOR A WINNER
    DSAME = same(TICTAC(1), TICTAC(5), TICTAC(9)) .OR. same(TICTAC(3), TICTAC(5), TICTAC(7))
    IF (DSAME) THEN
        WINNER = TICTAC(5)
        RETURN
    END IF
    ! NO WINNER AT ALL. SEE IF GAME IS A DRAW
    ! CHECK EACH ROW FOR AN EMPTY SPACE
    DO IR = 1, 9
        IF (TICTAC(IR) .EQ. BLANK) THEN
            OVER = .FALSE.
            RETURN
        END IF
    END DO
    !
    ! NO BLANK FOUND, GRAME IS A DRAW
    WINNER = DRAW
    !
    RETURN
END


! new version of chkplay, checks if user can move into a spot
LOGICAL FUNCTION CHKPLAY(TICTAC,MOVE)
    CHARACTER (len=1) TICTAC(9)
    INTEGER MOVE
    ! checks if the square is empty or not if so, it will return true
    select case (tictac(move))
    case(" ")
        CHKPLAY = .TRUE.
    case default
        CHKPLAY = .FALSE.
    end select
end

! checks if three variables are the same
logical function same(loc1, loc2, loc3)
    character (len=1) :: loc1,loc2,loc3
    same = .FALSE.
    !checks if three variables are the same, if so returns true
    if((loc1 .EQ. loc2) .and. (loc1 .EQ. loc3) .and. (loc1 .NE. " ")) then
        same = .TRUE.
    end if
end function same

! the main program
program tictactoe
! This program is a functioning tic-tac-toe
! between a human and computer
! Rhys Young
! 0925398
    call playTicTacToe()
end
