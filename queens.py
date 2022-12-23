

def nqueens(n):
    chess_matrix = [["0" for i in range (n)] for i in range(n)]
    chess_matrix[3][3] = "q"
    def queen_coverage(row,col,c = 1,moved = 0,row_filled = False,col_filled = False,diac_filled = False,diar_filled = False):
        chess_matrix[row][col] = "-" if chess_matrix[row][col] != "q" else chess_matrix[row][col]
        if not row_filled:
            if col == n-1:
                c = -1
                col = col - moved
                moved = 0
            if col == 0 and moved != 0:
                return queen_coverage(row,col + moved,1,0,True)
            else:
                return queen_coverage(row,col+c,c,moved+1)
        if not col_filled:
            if row == n-1:
                c = -1
                row = row - moved
                moved = 0
            if row == 0 and moved != 0:
                return queen_coverage(row + moved,col,1,0,True,True)
            else:
                return queen_coverage(row+c,col,c,moved+1,True)
        if not diac_filled:
            if row == n-1 or col == n-1:
                c = -1
                row = row - moved
                col = col - moved
                moved = 0
            if (row == 0 or col == 0) and moved != 0:
                return queen_coverage(row + moved,col + moved,1,0,True,True,True)
            else:
                return queen_coverage(row+c,col+c,c,moved+1,True,True)
        if not diar_filled:
            if row == n-1 or col == n-1:
                c = -1
                row = row - moved
                col = col - moved
                moved = 0
            if row == 0 or col == 0:
                return queen_coverage(row + moved,col - moved,1,0,True,True,True,True)
            else:
                return queen_coverage(row-c,col+c,c,moved+1,True,True,True)
        else:
            if row-2 > 0 and row-2 < n-1 and col+1 > 0 and col +1 < n-1 :
                chess_matrix[row-2][col+1] = "-"
            if row-2 > 0 and row-2 < n-1 and col-1 > 0 and col -1 < n-1 :
                chess_matrix[row-2][col-1] = "-"
            if row-1 > 0 and row-1 < n-1 and col-2 > 0 and col -2 < n-1 :
                chess_matrix[row-1][col-2] = "-"
            if row+1 > 0 and row+1 < n-1 and col-2 > 0 and col -2 < n-1 :
                chess_matrix[row+1][col-2] = "-"
            if row+2 > 0 and row+2 < n-1 and col-1 > 0 and col -1 < n-1 :
                chess_matrix[row+2][col-1] = "-"
            if row+2 > 0 and row+2 < n-1 and col+1 > 0 and col +1 < n-1 :
                chess_matrix[row+2][col+1] = "-"
            if row+1 > 0 and row+1 < n-1 and col+2 > 0 and col +2 < n-1 :
                chess_matrix[row+1][col+2] = "-"
            if row-1 > 0 and row-1 < n-1 and col+2 > 0 and col +2 < n-1 :
                chess_matrix[row-1][col+2] = "-"
        return chess_matrix
    return queen_coverage(3,3)

#print(nqueens(8))


def chess_matrix(matrix):
    string_matrix = "\n"
    for row in matrix:
        string_matrix += str(row) + "\n"
    return string_matrix[:-1]

print(chess_matrix(nqueens(8)))