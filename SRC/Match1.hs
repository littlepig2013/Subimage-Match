module Main where
    import Data.ByteString as ByteString
    import Data.Array
    import Data.Word
    import System.IO
    import Data.List
    import System.Environment
    import Codec.BMP
    main :: IO()
    main = do
        args <- getArgs
        Right bmpOrigin <- readBMP(Prelude.head args)
        Right bmpPattern <- readBMP(args!!1)
        ouh <- openFile "ref.txt" WriteMode
        let {rgbaOrigin = unpackBMPToRGBA32 bmpOrigin; 
            rgbaPattern = unpackBMPToRGBA32 bmpPattern;
            (widthOrigin, heightOrigin) = bmpDimensions bmpOrigin;
            (widthPattern, heightPattern) = bmpDimensions bmpPattern;
            matchMode = if (Prelude.length args) < 3 
                then (selectMode "0" rgbaOrigin rgbaPattern widthOrigin heightOrigin widthPattern heightPattern ) 
                else (selectMode (args!!2) rgbaOrigin rgbaPattern widthOrigin heightOrigin widthPattern heightPattern)} in
            --System.IO.hPutStrLn ouh $ show $ filterSaltAndPepper (byteStringToDustArray rgbaPattern widthPattern heightPattern) widthPattern heightPattern
            System.IO.hPutStrLn ouh $ show $ matchMode widthOrigin heightOrigin widthPattern heightPattern
        hClose ouh

    selectMode :: [Char] -> ByteString -> ByteString -> Int -> Int -> Int -> Int -> Int ->  Int -> Int -> Int -> (Int, Int)
    selectMode xs rgbaOrigin rgbaPattern widthOrigin heightOrigin widthPattern heightPattern
        | xs == "0" = matchWithoutDisturb arrayOrigin arrayPattern
        | xs == "1" = matchWithLineVague arrayDustOrigin arrayDustPattern
        | xs == "2" = matchWithColorDeepen arrayOrigin arrayPattern
        | xs == "3" = matchWithSPNoise arrayDustOrigin arrayDustPattern
        | xs == "4" = matchWithBlackPollute arrayOrigin arrayPattern
        | otherwise = matchWithoutDisturb arrayOrigin arrayPattern where
            arrayOrigin = byteStringToArray rgbaOrigin widthOrigin heightOrigin
            arrayPattern = byteStringToArray rgbaPattern widthPattern heightPattern
            arrayDustOrigin = byteStringToDustArray rgbaOrigin widthOrigin heightOrigin
            arrayDustPattern = byteStringToDustArray rgbaPattern widthPattern heightPattern

    getDustArray :: ByteString -> [Word8] -> [Word8]
    getDustArray xs ys = if len <= 4 then result else (getDustArray nextString result) where
        len = ByteString.length xs
        getDustValue (x1:x2:x3:xs) = let {
                y1 = fromEnum x1;
                y2 = fromEnum x2;
                y3 = fromEnum x3;
            } in x2 -- (x1 + x2 + x3) `div` 3 -- (x1*299 + x2*587 + x3*114) `div` 1000
        result = (getDustValue $ unpack (ByteString.take 4 xs)):ys
        nextString = ByteString.drop 4 xs

    byteStringToArray :: ByteString -> Int -> Int -> Array Int (Array Int Word8)
    byteStringToArray xs width height = listArray (1, height) arrayLine where
        arrayLine = Prelude.reverse $ getLineArray xs width []

    byteStringToDustArray :: ByteString -> Int -> Int -> Array Int (Array Int Word8)
    byteStringToDustArray xs width height = listArray (1, height) arrayLine where
        arrayLine = Prelude.reverse $ getDustLineArray xs width []

    getLineArray :: ByteString -> Int -> [Array Int Word8] -> [Array Int Word8]
    getLineArray xs len ys = if xLength <= realLen then result else (getLineArray rmnXs len result) where
        xLength = ByteString.length xs
        realLen = 4*len
        xsPreLen = unpack $ ByteString.take realLen xs
        rmnXs = ByteString.drop realLen xs
        result = (listArray (1, realLen) xsPreLen):ys

    getDustLineArray :: ByteString -> Int -> [Array Int Word8] -> [Array Int Word8]
    getDustLineArray xs len ys = if xLength <= realLen then result else (getDustLineArray rmnXs len result) where
        xLength = ByteString.length xs
        realLen = 4*len
        xsPreLen = ByteString.take realLen xs
        rmnXs = ByteString.drop realLen xs
        result = (listArray (1, len) (Prelude.reverse $ getDustArray xsPreLen [])):ys

    --match without any disturb
    matchWithoutDisturb :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) -> Int ->  Int -> Int -> Int -> (Int, Int)
    matchWithoutDisturb xs ys width height widthPattern heightPattern
        | len == 0 = error "No matching"
        | len == 1 = convertHeight $ Prelude.head result
        | otherwise = convertHeight $ Prelude.head $ [item| item <- result, matchAccuratePoint xs ys (width, height) (widthPattern, heightPattern) item] where
            lmtWidth = width - widthPattern
            lmtHeight = height - heightPattern
            result = matchWDLineCPResult xs ys 1 (lmtWidth, lmtHeight) width widthPattern 1
            len = Prelude.length result
            convertHeight temp = ((fst temp - 2), (height - (snd temp) - heightPattern + 1))

    matchWDLineCPResult :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) -> Int -> (Int, Int)  -> Int -> Int -> Int -> [(Int, Int)]
    matchWDLineCPResult xs ys curHeight (lmtWidth, lmtHeight)  orgnWidth ptnWidth curPtnHeight =
        let result = matchWDInOneLine xs ys (1, curHeight) (lmtWidth, lmtHeight) orgnWidth ptnWidth curPtnHeight in
        if curHeight == lmtHeight then result else result ++ (matchWDLineCPResult xs ys (curHeight + 1) (lmtWidth, lmtHeight) orgnWidth ptnWidth curPtnHeight)

    matchAccuratePoint :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
    matchAccuratePoint xs ys (orgnWidth,orgnHeight) (ptnWidth,ptnHeight) (x, y)
        = if (x > orgnWidth - ptnWidth || y > orgnHeight - ptnHeight) then False
            else and [(equal iTemp jTemp)|iTemp <- [1..ptnHeight], jTemp <- [1..ptnWidth]] where
            equal i j = let {xsLine = xs!(y + i - 1);ysLine = ys!i;actualX = 4*(x + j - 1) - 3 ;actualY = 4*j - 3} in
                (xsLine!actualX == ysLine!actualY) && (xsLine!(actualX + 1) == ysLine!(actualY + 1)) &&
                (xsLine!(actualX + 2) == ysLine!(actualY + 2))

    matchWDInOneLine :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) ->  (Int, Int) -> (Int, Int) -> Int -> Int -> Int-> [(Int, Int)]
    matchWDInOneLine xs ys (curWidth, curHeight) (lmtWidth,lmtHeight) orgnWidth ptnWidth curPtnHeight
        | curHeight > lmtHeight = []
        | curWidth == lmtWidth = result
        | curWidth < lmtWidth = result ++ (matchWDInOneLine xs ys ((curWidth + 1), curHeight) (lmtWidth, lmtHeight) orgnWidth ptnWidth curPtnHeight) where
            result = if and [(equal i)| i <- [1..ptnWidth]]
                then [(curWidth, curHeight)] else []
            equal j = let {xsLine = xs!curHeight;ysLine = ys!curPtnHeight;actualX = 4*(curWidth + j - 1) - 3 ;actualY = 4*j - 3} in
                (xsLine!actualX == ysLine!actualY) && (xsLine!(actualX + 1) == ysLine!(actualY + 1)) &&
                (xsLine!(actualX + 2) == ysLine!(actualY + 2))

    --match with linear smoothing vagueness
    matchWithLineVague :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) -> Int ->  Int -> Int -> Int -> (Int, Int)
    matchWithLineVague xs ys width height widthPattern heightPattern
        | len == 0 = error "No matching"
        | len == 1 = convertHeight $ Prelude.head result
        | len >= 10000 = convertHeight $ snd resultMinimum2
        | otherwise = convertHeight $ snd resultMinimum where
            lmtWidth = width - widthPattern
            lmtHeight = height - heightPattern
            ysFilterLV = filterLineVague ys widthPattern heightPattern
            result = matchWithLVLineCPResult xs ysFilterLV 1 (lmtWidth, lmtHeight) width ((widthPattern - 2), (heightPattern - 2)) 1
            result2 = matchWithLVLineCPResult xs ysFilterLV 1 (lmtWidth, lmtHeight) width ((widthPattern - 2), (heightPattern - 2)) (heightPattern `div` 3)
            --result3 = matchWithLVLineCPResult xs ysFilterLV 1 (lmtWidth, lmtHeight) width ((widthPattern - 2), (heightPattern - 2)) (heightPattern `div` 2)
            len = Prelude.length result
            matchWithLVPoint i = matchWithSPPoint xs ysFilterLV (width, height) ((widthPattern - 2), (heightPattern - 2)) i
            getDeviationResult str x = if temp < 0 then str else ((temp, x):str) where temp = matchWithLVPoint x
            resultMinimum = Prelude.minimum $ Prelude.foldl getDeviationResult [] result
            resultMinimum2 = if Prelude.null tempArray then resultMinimum 
                else Prelude.minimum $ Prelude.foldl getDeviationResult [] tempArray where
                    tempArray = [item|item <- result, Prelude.elem item result2]
            --resultMinimum3 = if Prelude.null tempArray then resultMinimum2 else  Prelude.minimum $ Prelude.foldl getDeviationResult [] tempArray where
                --tempArray = [item|item <- result, Prelude.elem item result2, Prelude.elem item result3]
            convertHeight temp = ((fst temp - 2), (height - (snd temp) - heightPattern) + 2)

    filterLineVague :: Array Int (Array Int Word8) -> Int -> Int -> Array Int (Array Int Word8)
    filterLineVague xs width height = 
        listArray (1,(height - 2)) [(getLineArray i)| i <- [2..(height - 1)]] where
            getMidNum i j = let {
                thisLine = xs!i;
                upLine = xs!(i - 1);
                downLine = xs!(i + 1);
                xMid = fromEnum (thisLine!j);
                xLeft = fromEnum (thisLine!(j - 1));
                xRight = fromEnum (thisLine!(j + 1));
                xUp = fromEnum (upLine!j);
                xDown = fromEnum (downLine!j);
                xLeftUp = fromEnum (upLine!(j - 1));
                xRightUp = fromEnum (upLine!(j + 1));
                xLeftDown = fromEnum (downLine!(j - 1));
                xRightDown = fromEnum (downLine!(j + 1));
            } in toEnum $ (sum [xMid, xLeft, xRight, xUp, xDown, xLeftUp, xRightUp, xLeftDown, xRightDown]) `div` 9
            getLineArray item = listArray (1,(width - 2)) [(getMidNum item j)|j <- [2..(width - 1)]]

    matchWithLVLineCPResult :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) -> Int -> (Int, Int)  -> Int -> (Int, Int) -> Int -> [(Int, Int)]
    matchWithLVLineCPResult xs ys curHeight (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight =
        let result = matchWithLVInOneLine xs ys (1, curHeight) (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight in
        if curHeight == lmtHeight then result else result ++ (matchWithLVLineCPResult xs ys (curHeight + 1) (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight)

    matchWithLVInOneLine :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) ->  (Int, Int) -> (Int, Int) -> Int -> (Int, Int) -> Int-> [(Int, Int)]
    matchWithLVInOneLine xs ys (curWidth, curHeight) (lmtWidth,lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight
        | curHeight > lmtHeight = []
        | curWidth == lmtWidth = result
        | curWidth < lmtWidth = result ++ (matchWithLVInOneLine xs ys ((curWidth + 1), curHeight) (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight) where
            offset = 0
            dividend = 1
            ptnMinBound = min ptnWidth ptnHeight
            highBound = (ptnMinBound-1) `div` dividend + offset
            result = if and (Prelude.drop 45 $ Data.List.sort [(inRange i)| i <- [offset..highBound]])
                then [(curWidth, curHeight)] else []
            inRange j = let {
                xsLine = xs!curHeight;
                ysLine = ys!curPtnHeight;
                value1 = xsLine!(j + curWidth);
                value2 = ysLine!(j + 1);
                ans = if value1 > value2 then (value1 - value2) else (value2 - value1)
            } in ans <= 25

    matchWithLVPoint :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
    matchWithLVPoint xs ys (orgnWidth, orgnHeight) (ptnWidth, ptnHeight) (x, y)
        = if (x > orgnWidth - ptnWidth || y > orgnHeight - ptnHeight) then (-1)
            else sum ([(result i j) | i <- [1..ptnHeight], j <- [1..ptnWidth], let temp = pos i j in (temp <= highBound && temp >= lowBound)]) where
                lenPtn = ptnWidth*ptnHeight
                dividend = 20
                lowBound = 10
                highBound = (lenPtn-2) `div` dividend + lowBound
                pos i1 j1 = (i1 - 1)*ptnWidth + j1
                result item1 item2 = let {
                    xsLine = xs!(y + item1 - 1);
                    ysLine = ys!item1;
                    value1 = xsLine!(item2 + x - 1);
                    value2 = ysLine!item2;
                    temp = if value1 > value2 then (value1 - value2) else (value2 - value1)
                } in fromEnum temp

    --match with color change
    matchWithColorDeepen :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) -> Int ->  Int -> Int -> Int -> (Int, Int)
    matchWithColorDeepen xs ys width height widthPattern heightPattern
        | len == 0 = error "No matching"
        | len == 1 = convertHeight $ Prelude.head result
        | len >= 10000 = convertHeight $ snd resultMinimum2
        | otherwise = convertHeight $ snd resultMinimum where
            lmtWidth = width - widthPattern - 1
            lmtHeight = height - heightPattern - 1
            xsFilterCD = filterColorDeepen xs width height
            ysFilterCD = filterColorDeepen ys widthPattern heightPattern
            result = matchWithCDLineCPResult xsFilterCD ysFilterCD 1 (lmtWidth, lmtHeight) (width - 1) ((widthPattern - 2), (heightPattern - 2)) 1
            result2 = matchWithCDLineCPResult xsFilterCD ysFilterCD 1 (lmtWidth, lmtHeight) (width - 1) ((widthPattern - 2), (heightPattern - 2)) (heightPattern `div` 3)
            --result3 = matchWithCDLineCPResult xsFilterCD ysFilterCD 1 (lmtWidth, lmtHeight) (width - 1) ((widthPattern - 2), (heightPattern - 2)) (heightPattern `div` 2)
            len = Prelude.length result
            matchWithCDOnePoint i = matchWithCDPoint xsFilterCD ysFilterCD ((width - 1), height) ((widthPattern - 2), (heightPattern - 2)) i
            getDeviationResult str x = if temp < 0 then str else ((temp, x):str) where temp = matchWithCDOnePoint x
            resultMinimum = Prelude.minimum $ Prelude.foldl getDeviationResult [] result
            resultMinimum2 = if Prelude.null tempArray then resultMinimum 
                else Prelude.minimum $ Prelude.foldl getDeviationResult [] tempArray where
                    tempArray = [item|item <- result, Prelude.elem item result2]
            --resultMinimum3 = if Prelude.null tempArray then resultMinimum2 else  Prelude.minimum $ Prelude.foldl getDeviationResult [] tempArray where
                --tempArray = [item|item <- result, Prelude.elem item result2, Prelude.elem item result3]
            convertHeight temp = ((fst temp - 2), (height - (snd temp) - heightPattern) + 2)
    
    filterColorDeepen :: Array Int (Array Int Word8) -> Int -> Int -> Array Int (Array Int Word8)
    filterColorDeepen xs width height = 
        listArray (1,(height - 2)) [(getLineArray i)| i <- [2..(height - 1)]] where
            getMidNum i j = let {
                thisLine = xs!i;
                upLine = xs!(i - 1);
                downLine = xs!(i + 1);
                xMid = fromEnum (thisLine!j);
                xLeft = fromEnum (thisLine!(j - 1));
                xRight = fromEnum (thisLine!(j + 1));
                xUp = fromEnum (upLine!j);
                xDown = fromEnum (downLine!j);
                xLeftUp = fromEnum (upLine!(j - 1));
                xRightUp = fromEnum (upLine!(j + 1));
                xLeftDown = fromEnum (downLine!(j - 1));
                xRightDown = fromEnum (downLine!(j + 1));
                xMidDev = (xMid - average)^2;
                xLeftDev = (xLeft - average)^2;
                xRightDev = (xRight - average)^2;
                xUpDev = (xUp - average)^2;
                xDownDev = (xDown - average)^2;
                sumDev = (xMidDev + xLeftDev + xRightDev + xUpDev + xDownDev) `div` 5;
                var = sqrt $ realToFrac sumDev
            } in toEnum (fromEnum $ ceiling var)  
            getLineArray item = listArray (1,(width - 2)) [(getMidNum item j)|j <- [2..(width - 1)]]

    matchWithCDLineCPResult :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) -> Int -> (Int, Int)  -> Int -> (Int, Int) -> Int -> [(Int, Int)]
    matchWithCDLineCPResult xs ys curHeight (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight =
        let result = matchWithCDInOneLine xs ys (1, curHeight) (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight in
        if curHeight == lmtHeight then result else result ++ (matchWithCDLineCPResult xs ys (curHeight + 1) (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight)

    matchWithCDInOneLine :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) ->  (Int, Int) -> (Int, Int) -> Int -> (Int, Int) -> Int-> [(Int, Int)]
    matchWithCDInOneLine xs ys (curWidth, curHeight) (lmtWidth,lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight
        | curHeight > lmtHeight = []
        | curWidth == lmtWidth = result
        | curWidth < lmtWidth = result ++ (matchWithCDInOneLine xs ys ((curWidth + 1), curHeight) (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight) where
            offset = 0
            dividend = 1
            ptnMinBound = min ptnWidth ptnHeight
            highBound = (ptnMinBound-1) `div` dividend + offset
            result = if and (Prelude.drop 100 $ Data.List.sort [(inRange i)| i <- [offset..highBound]])
                then [(curWidth, curHeight)] else []
            inRange j = let {
                xsLine = xs!curHeight;
                ysLine = ys!curPtnHeight;
                value1 = xsLine!(j + curWidth);
                value2 = ysLine!(j + 1);
                ans = if value1 > value2 then (value1 - value2) else (value2 - value1)
            } in ans <= 30

    matchWithCDPoint :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
    matchWithCDPoint xs ys (orgnWidth, orgnHeight) (ptnWidth, ptnHeight) (x, y)
        = if (x > orgnWidth - ptnWidth || y > orgnHeight - ptnHeight) then (-1)
            else sum ([(result i j) | i <- [1..ptnHeight], j <- [1..ptnWidth], let temp = pos i j in (temp <= highBound && temp >= lowBound)]) where
                lenPtn = ptnWidth*ptnHeight
                dividend = 20
                lowBound = 10
                highBound = (lenPtn-2) `div` dividend + lowBound
                pos i1 j1 = (i1 - 1)*ptnWidth + j1
                result item1 item2 = let {
                    xsLine = xs!(y + item1 - 1);
                    ysLine = ys!item1;
                    value1 = xsLine!(item2 + x - 1);
                    value2 = ysLine!item2;
                    temp = if value1 > value2 then (value1 - value2) else (value2 - value1)
                } in fromEnum temp

    --match with salt and pepper noise
    matchWithSPNoise :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) -> Int ->  Int -> Int -> Int -> (Int, Int)
    matchWithSPNoise xs ys width height widthPattern heightPattern
        | len == 0 = error "No matching"
        | len == 1 = convertHeight $ Prelude.head result
        | len >= 10000 = convertHeight $ snd resultMinimum2
        | otherwise = convertHeight $ snd resultMinimum where
            lmtWidth = width - widthPattern
            lmtHeight = height - heightPattern
            ysFilterSP = filterSaltAndPepper ys widthPattern heightPattern
            result = matchWithSPLineCPResult xs ysFilterSP 1 (lmtWidth, lmtHeight) width ((widthPattern - 2), (heightPattern - 2)) 1
            result2 = matchWithSPLineCPResult xs ysFilterSP 1 (lmtWidth, lmtHeight) width ((widthPattern - 2), (heightPattern - 2)) (heightPattern `div` 3)
            --result3 = matchWithSPLineCPResult xs ysFilterSP 1 (lmtWidth, lmtHeight) width ((widthPattern - 2), (heightPattern - 2)) (heightPattern `div` 2)
            len = Prelude.length result
            matchWithSPOnePoint i = matchWithSPPoint xs ysFilterSP (width, height) ((widthPattern - 2), (heightPattern - 2)) i
            getDeviationResult str x = if temp < 0 then str else ((temp, x):str) where temp = matchWithSPOnePoint x
            resultMinimum = Prelude.minimum $ Prelude.foldl getDeviationResult [] result
            resultMinimum2 = if Prelude.null tempArray then resultMinimum else  Prelude.minimum $ Prelude.foldl getDeviationResult [] tempArray where
                tempArray = [item|item <- result, Prelude.elem item result2]
            --resultMinimum3 = if Prelude.null tempArray then resultMinimum2 else  Prelude.minimum $ Prelude.foldl getDeviationResult [] tempArray where
               -- tempArray = [item|item <- result, Prelude.elem item result2, Prelude.elem item result3]
            convertHeight temp = ((fst temp - 2), (height - (snd temp) - heightPattern) + 2)

    filterSaltAndPepper :: Array Int (Array Int Word8) -> Int -> Int -> Array Int (Array Int Word8)
    filterSaltAndPepper xs width height = 
        listArray (1,(height - 2)) [(getLineArray i)| i <- [2..(height - 1)]] where
            getMidNum i j = let {
                thisLine = xs!i;
                upLine = xs!(i - 1);
                downLine = xs!(i + 1);
                xMid = thisLine!j;
                xLeft = thisLine!(j - 1);
                xRight = thisLine!(j + 1);
                xUp = upLine!j;
                xDown = downLine!j;
                xLeftUp = upLine!(j - 1);
                xRightUp = upLine!(j + 1);
                xLeftDown = downLine!(j - 1);
                xRightDown = downLine!(j + 1);
            } in (Prelude.head $ Prelude.drop 2 $ Data.List.sort [xMid, xLeft, xRight, xUp, xDown])
            getLineArray item = listArray (1,(width - 2)) [(getMidNum item j)|j <- [2..(width - 1)]]

    matchWithSPLineCPResult :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) -> Int -> (Int, Int)  -> Int -> (Int, Int) -> Int -> [(Int, Int)]
    matchWithSPLineCPResult xs ys curHeight (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight =
        let result = matchWithSPInOneLine xs ys (1, curHeight) (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight in
        if curHeight == lmtHeight then result else result ++ (matchWithSPLineCPResult xs ys (curHeight + 1) (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight)

    matchWithSPInOneLine :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) ->  (Int, Int) -> (Int, Int) -> Int -> (Int, Int) -> Int-> [(Int, Int)]
    matchWithSPInOneLine xs ys (curWidth, curHeight) (lmtWidth,lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight
        | curHeight > lmtHeight = []
        | curWidth == lmtWidth = result
        | curWidth < lmtWidth = result ++ (matchWithSPInOneLine xs ys ((curWidth + 1), curHeight) (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight) where
            offset = 0
            dividend = 1
            ptnMinBound = min ptnWidth ptnHeight
            highBound = (ptnMinBound-1) `div` dividend + offset
            result = if and (Prelude.drop 45 $ Data.List.sort [(inRange i)| i <- [offset..highBound]])
                then [(curWidth, curHeight)] else []
            inRange j = let {
                xsLine = xs!curHeight;
                ysLine = ys!curPtnHeight;
                value1 = xsLine!(j + curWidth);
                value2 = ysLine!(j + 1);
                ans = if value1 > value2 then (value1 - value2) else (value2 - value1)
            } in ans <= 25

    matchWithSPPoint :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
    matchWithSPPoint xs ys (orgnWidth, orgnHeight) (ptnWidth, ptnHeight) (x, y)
        = if (x > orgnWidth - ptnWidth || y > orgnHeight - ptnHeight) then (-1)
            else sum ([(result i j) | i <- [1..ptnHeight], j <- [1..ptnWidth], let temp = pos i j in (temp <= highBound && temp >= lowBound)]) where
                lenPtn = ptnWidth*ptnHeight
                dividend = 20
                lowBound = 10
                pos i1 j1 = (i1 - 1)*ptnWidth + j1
                highBound = (lenPtn-2) `div` dividend + lowBound
                result item1 item2 = let {
                    xsLine = xs!(y + item1 - 1);
                    ysLine = ys!item1;
                    value1 = xsLine!(item2 + x - 1);
                    value2 = ysLine!item2;
                    temp = if value1 > value2 then (value1 - value2) else (value2 - value1)
                } in fromEnum temp

    --match with black pollution
    matchWithBlackPollute :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) -> Int ->  Int -> Int -> Int -> (Int, Int)
    matchWithBlackPollute xs ys width height widthPattern heightPattern
        | len == 0 = error "No matching"
        | len == 1 = convertHeight $ Prelude.head result
        | otherwise = convertHeight $ snd resultMinimum where
            lmtWidth = width - widthPattern
            lmtHeight = height - heightPattern
            result = matchWBPLineCPResult xs ys 1 (lmtWidth, lmtHeight) width widthPattern 1
            len = Prelude.length result
            matchBPOnePoint i = matchBlackPollutionPoint xs ys (width, height) (widthPattern, heightPattern) i
            getDeviationResult xs x = if temp < 0 then xs else ((temp, x):xs) where temp = matchBPOnePoint x
            resultMinimum = Prelude.minimum $ Prelude.foldl getDeviationResult [] result
            convertHeight temp = ((fst temp - 1), (height - (snd temp) - heightPattern + 1))

    matchWBPLineCPResult :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) -> Int -> (Int, Int)  -> Int -> Int -> Int -> [(Int, Int)]
    matchWBPLineCPResult xs ys curHeight (lmtWidth, lmtHeight)  orgnWidth ptnWidth curPtnHeight =
        let result = matchWBPInOneLine xs ys (1, curHeight) (lmtWidth, lmtHeight) orgnWidth ptnWidth curPtnHeight in
        if curHeight == lmtHeight then result else result ++ (matchWBPLineCPResult xs ys (curHeight + 1) (lmtWidth, lmtHeight) orgnWidth ptnWidth curPtnHeight)

    matchWBPInOneLine :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) ->  (Int, Int) -> (Int, Int) -> Int -> Int -> Int-> [(Int, Int)]
    matchWBPInOneLine xs ys (curWidth, curHeight) (lmtWidth,lmtHeight) orgnWidth ptnWidth curPtnHeight
        | curHeight > lmtHeight = []
        | curWidth == lmtWidth = result
        | curWidth < lmtWidth = result ++ (matchWBPInOneLine xs ys ((curWidth + 1), curHeight) (lmtWidth, lmtHeight) orgnWidth ptnWidth curPtnHeight) where
            result = if and [(equal i)| i <- [1..ptnWidth]]
                then [(curWidth, curHeight)] else []
            equal j = let {xsLine = xs!curHeight;ysLine = ys!curPtnHeight;actualX = 4*(curWidth + j - 1) - 3 ;actualY = 4*j - 3} in
                (xsLine!actualX >= ysLine!actualY) && (xsLine!(actualX + 1) >= ysLine!(actualY + 1)) &&
                (xsLine!(actualX + 2) >= ysLine!(actualY + 2))

    matchBlackPollutionPoint :: Array Int (Array Int Word8) -> Array Int (Array Int Word8) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
    matchBlackPollutionPoint xs ys (orgnWidth,orgnHeight) (ptnWidth,ptnHeight) (x, y)
        = if (x > orgnWidth - ptnWidth || y > orgnHeight - ptnHeight) then (-1)
            else sum [(result i j)|i <- [1..ptnHeight], j <- [1..ptnWidth], (lessThan i j)] where
            lenPtn = ptnWidth*ptnHeight
            arrayPtn = [0..(lenPtn-1)]
            pointOrigin = y*orgnWidth + x
            lessThan item1 item2 = let {
                    xsLine = xs!(y + item1 - 1);
                    ysLine = ys!item1;
                    actualX = 4*(x + item2 - 1) - 3;
                    actualY = 4*item2 - 3;
                    actualX0 = xsLine!(actualX);
                    actualX1 = xsLine!(actualX + 1);
                    actualX2 = xsLine!(actualX + 2);
                    actualY0 = ysLine!(actualY);
                    actualY1 = ysLine!(actualY + 1);
                    actualY2 = ysLine!(actualY + 2);
                } in
                (actualX0 >= actualY0) && (actualX1 >= actualY1) && (actualX2 >= actualY2)
            result item1 item2 = let {
                    xsLine = xs!(y + item1 - 1);
                    ysLine = ys!item1;
                    actualX = 4*(x + item2 - 1) - 3;
                    actualY = 4*item2 - 3;
                    actualX0 = xsLine!(actualX);
                    actualX1 = xsLine!(actualX + 1);
                    actualX2 = xsLine!(actualX + 2);
                    actualY0 = ysLine!(actualY);
                    actualY1 = ysLine!(actualY + 1);
                    actualY2 = ysLine!(actualY + 2);
                    result0 = if actualX0 == actualY0 then 0 else 1;
                    result1 = if actualX1 == actualY1 then 0 else 1;
                    result2 = if actualX2 == actualY2 then 0 else 1;
                } in
                result0 + result1 + result2
