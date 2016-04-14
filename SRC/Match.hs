module Main where
    import Control.Parallel
    import Control.Parallel.Strategies
    import Control.Exception
    import Data.ByteString as ByteString
    import Data.Array
    import Data.Word
    import Data.List
    import System.IO
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
            lenOrigin = widthOrigin*heightOrigin;
            lenPattern = widthPattern*heightPattern;
            matchMode = if (Prelude.length args) < 3 then (selectMode "0" rgbaOrigin rgbaPattern lenOrigin lenPattern) 
                else (selectMode (args!!2) rgbaOrigin rgbaPattern lenOrigin lenPattern) } in
            System.IO.hPutStrLn ouh $ show $ matchMode widthOrigin heightOrigin widthPattern heightPattern
        hClose ouh

    selectMode :: [Char] -> ByteString -> ByteString -> Int -> Int -> Int ->  Int -> Int -> Int -> (Int, Int)
    selectMode xs rgbaOrigin rgbaPattern lenOrigin lenPattern
        | xs == "0" = matchWithoutDisturb arrayOrigin arrayPattern
        | xs == "1" = matchWithLineVague arrayDustOrigin arrayDustPattern
        | xs == "2" = matchWithColorDeepen arrayOrigin arrayPattern
        | xs == "3" = matchWithSPNoise arrayDustOrigin arrayDustPattern
        | xs == "4" = matchWithBlackPollute arrayOrigin arrayPattern
        | otherwise = matchWithoutDisturb arrayOrigin arrayPattern where
            arrayOrigin = byteStringToArray rgbaOrigin
            arrayPattern = byteStringToArray rgbaPattern
            arrayDustOrigin = (listArray (1,lenOrigin) (Prelude.reverse $ getDustArray rgbaOrigin []))
            arrayDustPattern = (listArray (1,lenPattern) (Prelude.reverse $ getDustArray rgbaPattern []))
    
    getDustArray :: ByteString -> [Word8] -> [Word8]
    getDustArray xs ys = if len <= 4 then result else (getDustArray nextString result) where
        len = ByteString.length xs
        getDustValue (x1:x2:x3:xs) = let {
                y1 = fromEnum x1;
                y2 = fromEnum x2;
                y3 = fromEnum x3;
            } in x2
        result = (getDustValue $ unpack (ByteString.take 4 xs)):ys
        nextString = ByteString.drop 4 xs

    byteStringToArray :: ByteString -> Array Int Word8
    byteStringToArray xs = listArray (1, len) (unpack xs) where
        len = (ByteString.length xs)

    --match without any disturb
    matchWithoutDisturb :: Array Int Word8 -> Array Int Word8 -> Int ->  Int -> Int -> Int -> (Int, Int)
    matchWithoutDisturb xs ys width height widthPattern heightPattern
        | len == 0 = error "No matching"
        | len == 1 = convertHeight $ Prelude.head result
        | otherwise = convertHeight $ Prelude.head $ [item| item <- result, matchAccuratePoint xs ys (width, height) (widthPattern, heightPattern) item] where
            lmtWidth = width - widthPattern
            lmtHeight = height - heightPattern
            result = matchWDLineCPResult xs ys 1 (lmtWidth, lmtHeight) width widthPattern 1
            len = Prelude.length result
            convertHeight temp = ((fst temp - 1), (height - (snd temp) - heightPattern + 1))

    matchWDLineCPResult :: Array Int Word8 -> Array Int Word8 -> Int -> (Int, Int)  -> Int -> Int -> Int -> [(Int, Int)]
    matchWDLineCPResult xs ys curHeight (lmtWidth, lmtHeight)  orgnWidth ptnWidth curPtnHeight =
        let result = matchWDInOneLine xs ys (1, curHeight) (lmtWidth, lmtHeight) orgnWidth ptnWidth curPtnHeight in
        if curHeight == lmtHeight then result else result ++ (matchWDLineCPResult xs ys (curHeight + 1) (lmtWidth, lmtHeight) orgnWidth ptnWidth curPtnHeight)

    matchAccuratePoint :: Array Int Word8 -> Array Int Word8 -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
    matchAccuratePoint xs ys (orgnWidth,orgnHeight) (ptnWidth,ptnHeight) (x, y)
        = if (x > orgnWidth - ptnWidth || y > orgnHeight - ptnHeight) then False
            else and [(equal (offset i) i)|i <- arrayPtn] where
            lenPtn = ptnWidth*ptnHeight
            arrayPtn = [0..(lenPtn-1)]
            pointOrigin = y*orgnWidth + x
            offset item = (item `div` ptnWidth)*orgnWidth + (item `mod` ptnWidth) + pointOrigin
            equal item1 item2 = let {actualX = 4*item1 - 3 ;actualY = 4*item2 + 1} in
                (xs!actualX == ys!actualY) && (xs!(actualX + 1) == ys!(actualY + 1)) &&
                (xs!(actualX + 2) == ys!(actualY + 2))

    matchWDInOneLine :: Array Int Word8 -> Array Int Word8 ->  (Int, Int) -> (Int, Int) -> Int -> Int -> Int-> [(Int, Int)]
    matchWDInOneLine xs ys (curWidth, curHeight) (lmtWidth,lmtHeight) orgnWidth ptnWidth curPtnHeight
        | curHeight > lmtHeight = []
        | curWidth == lmtWidth = result
        | curWidth < lmtWidth = result ++ (matchWDInOneLine xs ys ((curWidth + 1), curHeight) (lmtWidth, lmtHeight) orgnWidth ptnWidth curPtnHeight) where
            offsetOrigin = orgnWidth*(curHeight - 1) + curWidth
            offsetPatten = (curPtnHeight - 1)*ptnWidth
            result = if and [(equal (i + offsetOrigin) (i + offsetPatten))| i <- [0..(ptnWidth-1)]]
                then [(curWidth, curHeight)] else []
            equal item1 item2 = let {actualX = 4*item1 - 3 ;actualY = 4*item2 + 1} in
                (xs!actualX == ys!actualY) && (xs!(actualX + 1) == ys!(actualY + 1)) &&
                (xs!(actualX + 2) == ys!(actualY + 2))

    --match with linear smoothing vagueness
    matchWithLineVague :: Array Int Word8 -> Array Int Word8 -> Int ->  Int -> Int -> Int -> (Int, Int)
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
            ---result3 = matchWithLVLineCPResult xs ysFilterLV 1 (lmtWidth, lmtHeight) width ((widthPattern - 2), (heightPattern - 2)) (heightPattern `div` 2)
            len = Prelude.length result
            matchWithLVOnePoint i = matchWithLVPoint xs ysFilterLV (width, height) ((widthPattern - 2), (heightPattern - 2)) i
            getDeviationResult str x = if temp < 0 then str else ((temp, x):str) where temp = matchWithLVOnePoint x
            resultMinimum = Prelude.minimum $ Prelude.foldl getDeviationResult [] result
            resultMinimum2 = if Prelude.null tempArray then resultMinimum 
                else Prelude.minimum $ Prelude.foldl getDeviationResult [] tempArray where
                    tempArray = [item|item <- result, Prelude.elem item result2]
            --resultMinimum3 = if Prelude.null tempArray then resultMinimum2 else  Prelude.minimum $ Prelude.foldl getDeviationResult [] tempArray where
                --tempArray = [item|item <- result, Prelude.elem item result2, Prelude.elem item result3]
            convertHeight temp = ((fst temp - 2), (height - (snd temp) - heightPattern) + 2)

    filterLineVague :: Array Int Word8 -> Int -> Int -> Array Int Word8
    filterLineVague xs width height = listArray (1,subLen) [(getMidNum i j)|i <- [2..(height-1)], j <- [2..(width-1)]] where
        subLen = (height - 2) * (width - 2)
        getMidNum i j = let {
            index = (i-1)*width + j;
            xMid = fromEnum (xs!index);
            xLeft = fromEnum (xs!(index - 1));
            xRight = fromEnum (xs!(index + 1));
            xUp = fromEnum (xs!(index - width));
            xDown = fromEnum (xs!(index + width));
        } in toEnum ((xMid + xLeft + xRight + xUp + xDown) `div` 5)

    matchWithLVLineCPResult :: Array Int Word8 -> Array Int Word8 -> Int -> (Int, Int)  -> Int -> (Int, Int) -> Int -> [(Int, Int)]
    matchWithLVLineCPResult xs ys curHeight (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight =
        let result = matchWithLVInOneLine xs ys (1, curHeight) (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight in
        if curHeight == lmtHeight then result else result ++ (matchWithLVLineCPResult xs ys (curHeight + 1) (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight)

    matchWithLVInOneLine :: Array Int Word8 -> Array Int Word8 ->  (Int, Int) -> (Int, Int) -> Int -> (Int, Int) -> Int-> [(Int, Int)]
    matchWithLVInOneLine xs ys (curWidth, curHeight) (lmtWidth,lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight
        | curHeight > lmtHeight = []
        | curWidth == lmtWidth = result
        | curWidth < lmtWidth = result ++ (matchWithLVInOneLine xs ys ((curWidth + 1), curHeight) (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight) where
            offset = 0
            dividend = 1
            offsetOrigin = orgnWidth*(curHeight - 1) + curWidth
            offsetPatten = (curPtnHeight - 1)*ptnWidth
            ptnMinBound = min ptnWidth ptnHeight
            highBound = (ptnMinBound-1) `div` dividend + offset
            result = if and (Prelude.drop 45 $ Data.List.sort [(inRange (i + offsetOrigin) (i + offsetPatten) 25)| i <- [offset..highBound]])
                then [(curWidth, curHeight)] else []
            inRange item1 item2 delta = let {
                value1 = xs!item1;
                value2 = ys!(item2 + 1);
                result = if value1 > value2 then (value1 - value2) else (value2 - value1)} in result <= delta

    matchWithLVPoint :: Array Int Word8 -> Array Int Word8 -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
    matchWithLVPoint xs ys (orgnWidth, orgnHeight) (ptnWidth, ptnHeight) (x, y)
        = if (x > orgnWidth - ptnWidth || y > orgnHeight - ptnHeight) then (-1)
            else sum ([(result (offset i) i) | i <- arrayPtn]) where
                lenPtn = ptnWidth*ptnHeight
                dividend = 20
                lowBound = 10
                highBound = (lenPtn-1) `div` dividend + lowBound
                arrayPtn = [lowBound..highBound]
                pointOrigin = (y - 1)*orgnWidth + x
                offset item = (item `div` ptnWidth)*orgnWidth + (item `mod` ptnWidth) + pointOrigin
                result item1 item2 = let {
                    value1 = xs!item1;
                    value2 = ys!(item2 + 1);
                    temp = if value1 > value2 then (value1 - value2) else (value2 - value1)
                } in fromEnum temp

    --match with color change
    matchWithColorDeepen :: Array Int Word8 -> Array Int Word8 -> Int ->  Int -> Int -> Int -> (Int, Int)
    matchWithColorDeepen xs ys width height widthPattern heightPattern
        | len == 0 = error "No matching"
        | len == 1 = convertHeight $ Prelude.head result
        | len >= 10000 = convertHeight $ snd resultMinimum2
        | otherwise = convertHeight $ snd resultMinimum where
            lmtWidth = width - widthPattern - 1
            lmtHeight = height - heightPattern - 1
            xsFilterCD = filterColorDeepen xs width height
            ysFilterCD = filterColorDeepen ys widthPattern heightPattern
            result = matchWithCDLineCPResult xsFilterCD ysFilterCD 1 (lmtWidth, lmtHeight) (width - 2) ((widthPattern - 2), (heightPattern - 2)) 1
            result2 = matchWithCDLineCPResult xsFilterCD ysFilterCD 1 (lmtWidth, lmtHeight) (width - 2) ((widthPattern - 2), (heightPattern - 2)) (heightPattern `div` 3)
            --result3 = matchWithCDLineCPResult xsFilterCD ysFilterCD 1 (lmtWidth, lmtHeight) (width - 1) ((widthPattern - 2), (heightPattern - 2)) (heightPattern `div` 2)
            len = Prelude.length result
            matchWithCDOnePoint i = matchWithCDPoint xsFilterCD ysFilterCD ((width - 2), (height - 2)) ((widthPattern - 2), (heightPattern - 2)) i
            getDeviationResult str x = if temp < 0 then str else ((temp, x):str) where temp = matchWithCDOnePoint x
            resultMinimum = Prelude.minimum $ Prelude.foldl getDeviationResult [] result
            resultMinimum2 = if Prelude.null tempArray then resultMinimum 
                else Prelude.minimum $ Prelude.foldl getDeviationResult [] tempArray where
                    tempArray = [item|item <- result, Prelude.elem item result2]
            --resultMinimum3 = if Prelude.null tempArray then resultMinimum2 else  Prelude.minimum $ Prelude.foldl getDeviationResult [] tempArray where
                --tempArray = [item|item <- result, Prelude.elem item result2, Prelude.elem item result3]
            convertHeight temp = ((fst temp - 2), (height - (snd temp) - heightPattern) + 2)

    filterColorDeepen :: Array Int Word8 -> Int -> Int -> Array Int Word8
    filterColorDeepen xs width height = listArray (1,subLen) [(getMidNum i j)|i <- [2..(height - 1)], j <- [2..(width - 1)]] where
        subLen = (height - 2) * (width - 2)
        getMidNum i j = let {
            index = (i-1)*width + j;
            xMid = fromEnum (xs!index);
            xLeft = fromEnum (xs!(index - 1));
            xRight = fromEnum (xs!(index + 1));
            xUp = fromEnum (xs!(index - width));
            xDown = fromEnum (xs!(index + width));
            average = fromEnum ((xMid + xLeft + xRight + xUp + xDown) `div` 5);
            xMidDev = (xMid - average)^2;
            xLeftDev = (xLeft - average)^2;
            xRightDev = (xRight - average)^2;
            xUpDev = (xUp - average)^2;
            xDownDev = (xDown - average)^2;
            sumDev = (xMidDev + xLeftDev + xRightDev + xUpDev + xDownDev) `div` 5;
            var = sqrt $ realToFrac sumDev
        } in toEnum (fromEnum $ ceiling var) 

    matchWithCDLineCPResult :: Array Int Word8 -> Array Int Word8 -> Int -> (Int, Int)  -> Int -> (Int, Int) -> Int -> [(Int, Int)]
    matchWithCDLineCPResult xs ys curHeight (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight =
        let result = matchWithCDInOneLine xs ys (1, curHeight) (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight in
        if curHeight == lmtHeight then result else result ++ (matchWithCDLineCPResult xs ys (curHeight + 1) (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight)

    matchWithCDInOneLine :: Array Int Word8 -> Array Int Word8 ->  (Int, Int) -> (Int, Int) -> Int -> (Int, Int) -> Int-> [(Int, Int)]
    matchWithCDInOneLine xs ys (curWidth, curHeight) (lmtWidth,lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight
        | curHeight > lmtHeight = []
        | curWidth == lmtWidth = result
        | curWidth < lmtWidth = result ++ (matchWithCDInOneLine xs ys ((curWidth + 1), curHeight) (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight) where
            offset = 0
            dividend = 1
            offsetOrigin = orgnWidth*(curHeight - 1) + curWidth
            offsetPatten = (curPtnHeight - 1)*ptnWidth
            ptnMinBound = min ptnWidth ptnHeight
            highBound = (ptnMinBound-1) `div` dividend + offset
            result = if and (Prelude.drop 100 $ Data.List.sort [(inRange (i + offsetOrigin) (i + offsetPatten) 30)| i <- [offset..highBound]])
                then [(curWidth, curHeight)] else []
            inRange item1 item2 delta = let {
                value1 = fromEnum (xs!item1);
                value2 = fromEnum (ys!(item2 + 1));
                result = if value1 > value2 then (value1 - value2) else (value2 - value1)} in result <= delta

    matchWithCDPoint :: Array Int Word8 -> Array Int Word8 -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
    matchWithCDPoint xs ys (orgnWidth, orgnHeight) (ptnWidth, ptnHeight) (x, y)
        = if (x > orgnWidth - ptnWidth || y > orgnHeight - ptnHeight) then (-1)
            else sum ([(result (offset i) i) | i <- arrayPtn]) where
                lenPtn = ptnWidth*ptnHeight
                dividend = 20
                lowBound = 10
                highBound = (lenPtn-2) `div` dividend + lowBound
                arrayPtn = [lowBound..highBound]
                pointOrigin = (y - 1)*orgnWidth + x
                offset item = (item `div` ptnWidth)*orgnWidth + (item `mod` ptnWidth) + pointOrigin
                result item1 item2 = let {
                    value1 = fromEnum (xs!item1);
                    value2 = fromEnum (ys!(item2 + 1));
                    result = if value1 > value2 then (value1- value2) else (value2 - value1)
                } in fromEnum result

    --match with salt and pepper noise
    matchWithSPNoise :: Array Int Word8 -> Array Int Word8 -> Int ->  Int -> Int -> Int -> (Int, Int)
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
                --tempArray = [item|item <- result, Prelude.elem item result2, Prelude.elem item result3]
            convertHeight temp = ((fst temp - 2), (height - (snd temp) - heightPattern) + 2)

    filterSaltAndPepper :: Array Int Word8 -> Int -> Int -> Array Int Word8
    filterSaltAndPepper xs width height = listArray (1,subLen) [(getMidNum i j)|i <- [2..(height-1)], j <- [2..(width-1)]] where
        subLen = (height - 2) * (width - 2)
        getMidNum i j = let {
            index = (i-1)*width + j;
            xMid = xs!index;
            xLeft = xs!(index - 1);
            xRight = xs!(index + 1);
            xUp = xs!(index - width);
            xDown = xs!(index + width);
            xLeftUp = xs!(index - 1 - width);
            xRightUp = xs!(index + 1 - width);
            xLeftDown = xs!(index - 1 + width);
            xRightDown = xs!(index + 1 + width);
        } in (Prelude.head $ Prelude.drop 2 $ Data.List.sort [xMid, xLeft, xRight, xUp, xDown])

    matchWithSPLineCPResult :: Array Int Word8 -> Array Int Word8 -> Int -> (Int, Int)  -> Int -> (Int, Int) -> Int -> [(Int, Int)]
    matchWithSPLineCPResult xs ys curHeight (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight =
        let result = matchWithSPInOneLine xs ys (1, curHeight) (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight in
        if curHeight == lmtHeight then result else result ++ (matchWithSPLineCPResult xs ys (curHeight + 1) (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight)

    matchWithSPInOneLine :: Array Int Word8 -> Array Int Word8 ->  (Int, Int) -> (Int, Int) -> Int -> (Int, Int) -> Int-> [(Int, Int)]
    matchWithSPInOneLine xs ys (curWidth, curHeight) (lmtWidth,lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight
        | curHeight > lmtHeight = []
        | curWidth == lmtWidth = result
        | curWidth < lmtWidth = result ++ (matchWithSPInOneLine xs ys ((curWidth + 1), curHeight) (lmtWidth, lmtHeight) orgnWidth (ptnWidth, ptnHeight) curPtnHeight) where
            offset = 0
            dividend = 1
            offsetOrigin = orgnWidth*(curHeight - 1) + curWidth
            offsetPatten = (curPtnHeight - 1)*ptnWidth
            ptnMinBound = min ptnWidth ptnHeight
            highBound = (ptnMinBound-1) `div` dividend + offset
            result = if and (Prelude.drop 45 $ Data.List.sort [(inRange (i + offsetOrigin) (i + offsetPatten) 25)| i <- [offset..highBound]])
                then [(curWidth, curHeight)] else []
            inRange item1 item2 delta = let {
                value1 = xs!item1;
                value2 = ys!(item2 + 1);
                ans = if value1 > value2 then (value1 - value2) else (value2 - value1)} in ans <= delta

    matchWithSPPoint :: Array Int Word8 -> Array Int Word8 -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
    matchWithSPPoint xs ys (orgnWidth, orgnHeight) (ptnWidth, ptnHeight) (x, y)
        = if (x > orgnWidth - ptnWidth || y > orgnHeight - ptnHeight) then (-1)
            else sum ([(result (offset i) i) | i <- arrayPtn]) where
                lenPtn = ptnWidth*ptnHeight
                dividend = 20
                lowBound = 10
                highBound = (lenPtn-1) `div` dividend + lowBound
                arrayPtn = [lowBound..highBound]
                pointOrigin = (y - 1)*orgnWidth + x
                offset item = (item `div` ptnWidth)*orgnWidth + (item `mod` ptnWidth) + pointOrigin
                result item1 item2 = let {
                    value1 = xs!item1;
                    value2 = ys!(item2 + 1);
                    temp = if value1 > value2 then (value1 - value2) else (value2 - value1)
                } in fromEnum temp

    --match with black pollution
    matchWithBlackPollute :: Array Int Word8 -> Array Int Word8 -> Int ->  Int -> Int -> Int -> (Int, Int)
    matchWithBlackPollute xs ys width height widthPattern heightPattern
        | len == 0 = error "No matching"
        | len == 1 = convertHeight $ Prelude.head result
        | otherwise = convertHeight $ snd resultMinimum where
            lmtWidth = width - widthPattern
            lmtHeight = height - heightPattern
            result = matchWBPLineCPResult xs ys 1 (lmtWidth, lmtHeight) width widthPattern 1
            len = Prelude.length result
            matchBPOnePoint i = matchBlackPollutionPoint xs ys (width, height) (widthPattern, heightPattern) i
            getDeviationResult str x = if temp < 0 then str else ((temp, x):str) where temp = matchBPOnePoint x
            resultMinimum = Prelude.minimum $ Prelude.foldl getDeviationResult [] result
            convertHeight temp = ((fst temp - 1), (height - (snd temp) - heightPattern + 1))

    matchWBPLineCPResult :: Array Int Word8 -> Array Int Word8 -> Int -> (Int, Int)  -> Int -> Int -> Int -> [(Int, Int)]
    matchWBPLineCPResult xs ys curHeight (lmtWidth, lmtHeight)  orgnWidth ptnWidth curPtnHeight =
        let result = matchWBPInOneLine xs ys (1, curHeight) (lmtWidth, lmtHeight) orgnWidth ptnWidth curPtnHeight in
        if curHeight == lmtHeight then result else result ++ (matchWBPLineCPResult xs ys (curHeight + 1) (lmtWidth, lmtHeight) orgnWidth ptnWidth curPtnHeight)

    matchWBPInOneLine :: Array Int Word8 -> Array Int Word8 ->  (Int, Int) -> (Int, Int) -> Int -> Int -> Int-> [(Int, Int)]
    matchWBPInOneLine xs ys (curWidth, curHeight) (lmtWidth,lmtHeight) orgnWidth ptnWidth curPtnHeight
        | curHeight > lmtHeight = []
        | curWidth == lmtWidth = result
        | curWidth < lmtWidth = result ++ (matchWBPInOneLine xs ys ((curWidth + 1), curHeight) (lmtWidth, lmtHeight) orgnWidth ptnWidth curPtnHeight) where
            offsetOrigin = orgnWidth*(curHeight - 1) + curWidth
            offsetPatten = (curPtnHeight - 1)*ptnWidth
            result = if and [(equal (i + offsetOrigin) (i + offsetPatten))| i <- [0..(ptnWidth-1)]]
                then [(curWidth, curHeight)] else []
            equal item1 item2 = let {actualX = 4*item1 - 3 ;actualY = 4*item2 + 1} in
                (xs!actualX >= ys!actualY) && (xs!(actualX + 1) >= ys!(actualY + 1)) &&
                (xs!(actualX + 2) >= ys!(actualY + 2))

    matchBlackPollutionPoint :: Array Int Word8 -> Array Int Word8 -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
    matchBlackPollutionPoint xs ys (orgnWidth,orgnHeight) (ptnWidth,ptnHeight) (x, y)
        = if (x > orgnWidth - ptnWidth || y > orgnHeight - ptnHeight) then (-1)
            else (sum [(result (offset i) i)|i <- arrayPtn, (lessThan (offset i) i)]) where
            lenPtn = ptnWidth*ptnHeight
            arrayPtn = [0..(lenPtn-1)]
            pointOrigin = y*orgnWidth + x
            offset item = (item `div` ptnWidth)*orgnWidth + (item `mod` ptnWidth) + pointOrigin
            lessThan item1 item2 = let {
                    actualX = 4*item1 - 3;
                    actualY = 4*item2 + 1;
                    actualX0 = xs!(actualX);
                    actualX1 = xs!(actualX + 1);
                    actualX2 = xs!(actualX + 2);
                    actualY0 = ys!(actualY);
                    actualY1 = ys!(actualY + 1);
                    actualY2 = ys!(actualY + 2);
            } in (actualX0 >= actualY0) && (actualX1 >= actualY1) && (actualX2 >= actualY2)
            result item1 item2 = let {
                    actualX = 4*item1 - 3;
                    actualY = 4*item2 + 1;
                    actualX0 = xs!(actualX);
                    actualX1 = xs!(actualX + 1);
                    actualX2 = xs!(actualX + 2);
                    actualY0 = ys!(actualY);
                    actualY1 = ys!(actualY + 1);
                    actualY2 = ys!(actualY + 2);
                    result0 = if actualX0 == actualY0 then 0 else 1;
                    result1 = if actualX1 == actualY1 then 0 else 1;
                    result2 = if actualX2 == actualY2 then 0 else 1;
            } in result0 + result1 + result2
