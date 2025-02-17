import entity

class UserLoginController:
    def __init__(self):
        self.userAccount = entity.UserAccount()

    def loginAccount(self, user_id, password):
        return self.userAccount.loginAccount(user_id, password)
    
class AdminViewAllUserAccountsController:
    def __init__(self):
        self.userAccount = entity.UserAccount()

    def viewAllUserAccounts(self):
        return self.userAccount.viewAllUserAccounts()

class AdminViewAccountInfoController:
    def __init__(self):
        self.userAccount = entity.UserAccount()

    def viewAccountInfo(self, user_id):
        return self.userAccount.viewAccountInfo(user_id)

class AdminCreateUserAccountController:
    def __init__(self):
        self.userAccount = entity.UserAccount()

    def createUserAccount(self,user_id,name,selectedProfile,password,email,phoneNum,suspend):
        return self.userAccount.createUserAccount(user_id,name,selectedProfile,password,email,phoneNum,suspend)
    
class AdminUpdateUserAccountController:
    def __init__(self):
        self.userAccount = entity.UserAccount()
    
    def updateUserAccount(self, user_id, name, password, email, phone):
        return self.userAccount.updateUserAccount(user_id, name, password, email, phone)
    
class AdminSuspendUserAccountController:
    def __init__(self):
        self.userAccount = entity.UserAccount()

    def suspendUserAccount(self, user_id):
        return self.userAccount.suspendUserAccount(user_id)

class AdminUnsuspendUserAccountController:
    def __init__(self):
        self.userAccount = entity.UserAccount()

    def unsuspendUserAccount(self, user_id, profileName):
        return self.userAccount.unsuspendUserAccount(user_id, profileName)

class AdminSearchUserAccountController:
    def __init__(self):
        self.userProfile = entity.UserAccount()
    
    def searchUserAccount(self, user_id):
        return self.userProfile.searchUserAccount(user_id)

class AdminViewAllUserProfilesController:
    def __init__(self):
        self.userProfile = entity.UserProfile()

    def viewAllUserProfiles(self):
        return self.userProfile.viewAllUserProfiles()

class AdminCreateUserProfileController:
    def __init__(self):
        self.userProfile = entity.UserProfile()
    def createUserProfile(self,profileName,description,suspend):
        return self.userProfile.createUserProfile(profileName,description,suspend)

class AdminViewProfileInfoController:
    def __init__(self):
        self.userProfile = entity.UserProfile()

    def viewProfileInfo(self, profileName):
        return self.userProfile.viewProfileInfo(profileName)

class AdminUpdateUserProfileController:
    def __init__(self):
        self.userProfile = entity.UserProfile()
    
    def updateUserProfile(self, profileName, description):
        return self.userProfile.updateUserProfile(profileName, description)
        
class AdminSuspendUserProfileController:
    def __init__(self):
        self.userProfile = entity.UserProfile()

    def suspendUserProfile(self, profileName):
        return self.userProfile.suspendUserProfile(profileName)
    
class AdminUnsuspendUserProfileController:
    def __init__(self):
        self.userProfile = entity.UserProfile()

    def unsuspendUserProfile(self, profileName):
        return self.userProfile.unsuspendUserProfile(profileName)

class AdminSearchUserProfileController:
    def __init__(self):
        self.userProfile = entity.UserProfile()
    
    def searchUserProfile(self, profileName):
        return self.userProfile.searchUserProfile(profileName)
    
class SystemFetchUserProfilesController:
    def __init__(self):
        self.userProfile = entity.UserProfile()
    
    def fetchUserProfiles(self):
        return self.userProfile.fetchUserProfiles() 

class AgentViewAllPropertyListingsController:
    def __init__(self):
        self.propertyListing = entity.PropertyListing()

    def viewAllPropertyListings(self):
        return self.propertyListing.viewAllPropertyListings() 

class AgentViewMyPropertyListingsController:
    def __init__(self):
        self.PropertyListing = entity.PropertyListing()

    def viewMyPropertyListings(self, agent_id):
        return self.PropertyListing.agentViewMyPropertyListings(agent_id)

class AgentViewListingInfoController:
    def __init__(self):
        self.listing = entity.PropertyListing()

    def viewListingInfo(self, listing_id):
        return self.listing.viewListingInfo(listing_id)

class AgentCreatePropertyListingController:
    def __init__(self):
        self.propertyListing = entity.PropertyListing()
    
    def createPropertyListing(self, title, status, img, agent_id, seller_id, price, description, suspend):
        return self.propertyListing.createPropertyListing(title, status, img, agent_id, seller_id, price, description, suspend)
    
class AgentUpdatePropertyListingController:
    def __init__(self):
        self.propertyListing = entity.PropertyListing()

    def updatePropertyListing(self, listing_id, title, status, seller_id, price, description):
        return self.propertyListing.updatePropertyListing(listing_id, title, status, seller_id, price, description)
    
class AgentSuspendPropertyListingController:
    def __init__(self):
        self.property = entity.PropertyListing()

    def suspendPropertyListing(self, listing_id):
        return self.property.suspendPropertyListing(listing_id)
    
class AgentUnsuspendPropertyListingController:
    def __init__(self):
        self.property = entity.PropertyListing()

    def unsuspendPropertyListing(self, listing_id):
        return self.property.unsuspendPropertyListing(listing_id)

class SearchPropertyListingController:
    def __init__(self):
        self.listing = entity.PropertyListing()

    def searchPropertyListing(self, title):
        return self.listing.searchPropertyListing(title)

class AgentViewRatingController:
    def __init__(self):
        self.rating = entity.Rating()

    def viewRatings(self, agent_id):
        return self.rating.viewRatings(agent_id)
    
class AgentViewReviewController:
    def __init__(self):
        self.review = entity.Review()

    def viewReview(self, agent_id):
        return self.review.viewReview(agent_id)

class BuyerViewAllPropertyListingsController:
    def __init__(self):
        self.propertylisting = entity.PropertyListing()

    def viewAllListings(self, status):
        return self.propertylisting.buyerViewAllPropertyListings(status)
    
class BuyerViewSavedPropertyListingsController:
    def __init__(self):
        self.propertylisting = entity.PropertyListing()

    def viewSavedListings(self, buyer_id):
        return self.propertylisting.buyerViewSavedPropertyListings(buyer_id)

class BuyerSearchPropertyListingController:
    def __init__(self):
        self.listing = entity.PropertyListing()

    def searchPropertyListing(self, title, status):
        return self.listing.buyerSearchPropertyListing(title, status)

class BuyerViewListingInfoController:
    def __init__(self):
        self.listing = entity.PropertyListing()

    def viewListingInfo(self, listing_id, buyer_id):
        return self.listing.buyerViewListingInfo(listing_id, buyer_id)

class BuyerShortlistPropertyListingController:
    def __init__(self):
        self.shortlisting = entity.PropertyListing()

    def shortlistProperty(self, buyer_id, listing_id):
        return self.shortlisting.shortlistProperty(buyer_id, listing_id)

class BuyerUnshortlistPropertyListingController:
    def __init__(self):
        self.shortlisting = entity.PropertyListing()

    def unshortlistProperty(self, buyer_id, listing_id):
        return self.shortlisting.unshortlistProperty(buyer_id, listing_id)

class SellerViewAllPropertyListingsController:
    def __init__(self):
        self.PropertyListing = entity.PropertyListing()

    def viewAllPropertyListings(self):
        return self.PropertyListing.viewAllPropertyListings()
    
class SellerViewMyPropertyListingsController:
    def __init__(self):
        self.PropertyListing = entity.PropertyListing()

    def viewMyPropertyListings(self, seller_id):
        return self.PropertyListing.sellerViewMyPropertyListings(seller_id)

class SellerViewListingInfoController:
    def __init__(self):
        self.PropertyListing = entity.PropertyListing()

    def viewListingInfo(self, listing_id):
        return self.PropertyListing.viewListingInfo(listing_id)

class CustomerViewAgentInfoController:
    def __init__(self):
        self.userAccount = entity.UserAccount()

    def viewAgentInfo(self, agent_id):
        return self.userAccount.viewAccountInfo(agent_id)

class CustomerRateAgentController:
    def __init__(self):
        self.rating = entity.Rating()

    def rateAgent(self, customer_id, agent_id, rating):
        return self.rating.createRating(customer_id, agent_id, rating)
    
class CustomerReviewAgentController:
    def __init__(self):
        self.review = entity.Review()

    def reviewAgent(self, customer_id, agent_id, review):
        return self.review.createReview(customer_id, agent_id, review)

class UserUpdatePasswordController:
    def __init__(self):
        self.userAccount = entity.UserAccount()

    def updatePassword(self, user_id, new_password):
        return self.userAccount.updatePassword(user_id, new_password)

class UserViewMyAccountController:
    def __init__(self):
        self.userAccount = entity.UserAccount()

    def viewMyAccount(self, user_id):
        return self.userAccount.viewAccountInfo(user_id)
