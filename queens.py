def nqueens(n):
    def queen_coverage(row,col,matrix,c = 1,filled = 0,moved = 0,row_filled = False,col_filled = False,diac_filled = False,diar_filled = False):
        if matrix[row][col] == " ":
            matrix[row][col] = "-"
        if not row_filled:
            if col == 0:
                filled += 1
            if col == n-1:
                c = -1
                filled += 1
                col = col - moved
                moved = 0
            if filled == 2:
                return queen_coverage(row,col + moved,matrix,1,0,0,True)
            return queen_coverage(row,col + c,matrix,c,filled,moved+1)
        if not col_filled:
            if row == 0:
                filled += 1
            if row == n-1:
                c = -1
                filled += 1
                row = row - moved
                moved = 0
            if filled == 2:
                return queen_coverage(row + moved,col,matrix,1,0,0,True,True)
            return queen_coverage(row + c,col,matrix,c,filled,moved+1,True)
        if not diac_filled:
            if col == 0 or row == 0:
                filled += 1
            if col == n-1 or row == n-1:
                c = -1
                filled += 1
                col = col - moved
                row = row - moved
                moved = 0
            if filled == 2:
                return queen_coverage(row + moved,col + moved,matrix,1,0,0,True,True,True)
            return queen_coverage(row + c,col + c,matrix,c,filled,moved+1,True,True)
        if not diar_filled:
            if row == 0 or col == n-1:
                filled += 1
            if row == n-1 or col == 0:
                c = -1
                filled += 1
                row = row - moved
                col = col + moved
                moved = 0
            if filled == 2:
                return queen_coverage(row + moved,col - moved,matrix,1,0,0,True,True,True,True)
            return queen_coverage(row + c ,col - c,matrix,c,filled,moved+1,True,True,True)
        matrix[row][col] = "q"
        return matrix
    
    def next_queen(r,c,m):
        if r >= n:
            return [r,c,0]
        if m[r][c] == " ":
            return [r,c,1]
        if c == n-1:
            return [r,c,0]
        return next_queen(r,c+1,m)


    def find_queens(r=0,c=0,m = [[" " for i in range(n)] for i in range(n)],q = 0,l = [],d = [False]):
        queen_pos = next_queen(r,c,m) # find queen in row
        e = str(m) # store old matrix
        if q == n: # if queens found return matrix
            d[0] = True
            l += [m]
            return l[0]
        if queen_pos[2] == 0:
            return # if no queen found backtrack (to find n queens there has to be one queen in each row)
        find_queens(queen_pos[0]+1,0,queen_coverage(queen_pos[0],queen_pos[1],m),q+1) #build new queen matrix
        while d[0] != True:
            if queen_pos[1] == n-1: #if spot is already last in row backtrack
                return
            queen_pos = next_queen(queen_pos[0],queen_pos[1]+1,eval(e)) # if n queens werent found try placing queen to the right
            if queen_pos[2] == 0: # if spot in row wasnt found backtrack
                return
            find_queens(queen_pos[0]+1,0,queen_coverage(queen_pos[0],queen_pos[1],eval(e)),q+1) # try to find with new spot
        return l[0]

    return find_queens()






def chess_matrix2(matrix):
    string_matrix = "\n"
    for row in matrix:
        string_matrix += str(row) + "\n"
    return string_matrix[:-1]

print(chess_matrix2(nqueens(8)))