-- LeetCode problem
{- 
You are given two non-empty linked lists representing two non-negative integers. 
The digits are stored in reverse order, and each of their nodes contains a single digit.
Add the two numbers and return the sum as a linked list.

You may assume the two numbers do not contain any leading zero, 
except the number 0 itself.-}


-- Definition of a singly-linked list
data ListNode = ListNode { val :: Int, next :: Maybe ListNode }
-- ListNode represents a node in a singly-linked list
-- each node has a value, and a next which references the next node
-- next is wrapped in maybe because last node will have a reference of nothing
-- Function to add two linked lists representing reversed numbers

addTwoNumbers :: Maybe ListNode -> Maybe ListNode -> Maybe ListNode
addTwoNumbers l1 l2 = addTwoNumbersHelper l1 l2 0

-- The above function takes two lists in reverse order
-- call another helper function with the input of the
-- linked list an initial curry of 0

-- Helper function with carry
addTwoNumbersHelper :: Maybe ListNode -> Maybe ListNode -> Int -> Maybe ListNode
addTwoNumbersHelper Nothing Nothing 0 = Nothing
-- if linked lists are empty then no calculation takes place
addTwoNumbersHelper (Just n1) Nothing carry = 
    let sum = val n1 + carry
        (newVal, newCarry) = if sum >= 10 then (sum - 10, 1) else (sum, 0)
    in Just (ListNode newVal (addTwoNumbersHelper (next n1) Nothing newCarry))
-- the above does the calculation when first linked list is not empty but the second is
-- calculates the sum of the current digit from the first Linked list and carry
-- if sum is greater than or equal to 10, it reduces sum by 10 and sets carry to 1
-- creates a new node with newVal and computes the addition recursively
addTwoNumbersHelper Nothing (Just n2) carry = 
    let sum = val n2 + carry
        (newVal, newCarry) = if sum >= 10 then (sum - 10, 1) else (sum, 0)
    in Just (ListNode newVal (addTwoNumbersHelper Nothing (next n2) newCarry))
addTwoNumbersHelper (Just n1) (Just n2) carry = 
    let sum = val n1 + val n2 + carry
        (newVal, newCarry) = if sum >= 10 then (sum - 10, 1) else (sum, 0)
    in Just (ListNode newVal (addTwoNumbersHelper (next n1) (next n2) newCarry))

-- Convert a list of digits to a linked list
digitsToLinkedList :: [Int] -> Maybe ListNode
digitsToLinkedList [] = Nothing
digitsToLinkedList (x:xs) = Just (ListNode x (digitsToLinkedList xs))

-- Convert a linked list to a list of digits
linkedListToDigits :: Maybe ListNode -> [Int]
linkedListToDigits Nothing = []
linkedListToDigits (Just node) = val node : linkedListToDigits (next node)

main :: IO ()
main = do
    let num1 = digitsToLinkedList [2, 4, 3] -- Represents the number 342
    let num2 = digitsToLinkedList [5, 6, 4] -- Represents the number 465
    
    let result = addTwoNumbers num1 num2
    
    putStrLn $ "Result: " ++ show (linkedListToDigits result)
